package org.jetbrains.plugins.scala
package lang
package completion

import com.intellij.codeInsight.completion._
import com.intellij.openapi.project.Project
import com.intellij.patterns.PlatformPatterns.psiElement
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil.getContextOfType
import com.intellij.util.{Consumer, ProcessingContext}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.completion.ScalaAfterNewCompletionContributor._
import org.jetbrains.plugins.scala.lang.completion.ScalaCompletionUtil._
import org.jetbrains.plugins.scala.lang.completion.lookups.ScalaLookupItem
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes.{tMULTILINE_STRING, tSTRING}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil.getCompanionModule
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScConstructorPattern
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScReference, ScStableCodeReference}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAlias
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTrait}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClasses
import org.jetbrains.plugins.scala.lang.psi.impl.{ScalaPsiElementFactory, ScalaPsiManager}
import org.jetbrains.plugins.scala.lang.psi.light.PsiClassWrapper
import org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult

class ScalaClassNameCompletionContributor extends ScalaCompletionContributor {

  import ScalaClassNameCompletionContributor._

  extend(
    CompletionType.BASIC,
    identifierWithParentPattern(classOf[ScReference]),
    new CompletionProvider[CompletionParameters] {

      override def addCompletions(parameters: CompletionParameters,
                                  context: ProcessingContext,
                                  result: CompletionResultSet): Unit = {
        completeClassName(result)(parameters, context)
        result.stopHere()
      }
    }
  )

  extend(
    CompletionType.BASIC,
    psiElement,
    new CompletionProvider[CompletionParameters]() {

      override def addCompletions(parameters: CompletionParameters,
                                  context: ProcessingContext,
                                  result: CompletionResultSet): Unit =
        parameters.getPosition.getNode.getElementType match {
          case `tSTRING` | `tMULTILINE_STRING` =>
            completeClassName(result)(parameters, context)
          case _ =>
        }
    }
  )
}

object ScalaClassNameCompletionContributor {

  private[this] final case class CompletionState(place: PsiElement,
                                                 invocationCount: Int,
                                                 isInSimpleString: Boolean,
                                                 isInImport: Boolean,
                                                 isInStableCodeReference: Boolean,
                                                 classesOnly: Boolean,
                                                 annotationsOnly: Boolean) {

    val renamesMap: RenamesMap = createRenamesMap(place)

    def isValidClass(`class`: PsiClass): Boolean =
      isValidAndAccessible(`class`) &&
        (!annotationsOnly || `class`.isAnnotationType) &&
        isApplicable(`class`) &&
        !isExcluded(`class`)

    def createLookupItem(`class`: PsiClass,
                         maybeExpectedTypes: Option[(PsiClass, RenamesMap) => ScalaLookupItem]): ScalaLookupItem =
      maybeExpectedTypes match {
        case Some(createLookups) => createLookups(`class`, renamesMap)
        case _ => createLookupItemImpl(`class`)
      }

    def isValidAlias(alias: ScTypeAlias): Boolean =
      !annotationsOnly &&
        (isInImport || classesOnly) &&
        isValidAndAccessible(alias) &&
        !Option(alias.containingClass).exists(isExcluded)

    def createLookupItem(alias: ScTypeAlias): ScalaLookupItem =
      createLookupItemImpl(alias)

    private[this] def createLookupItemImpl(element: PsiNamedElement): ScalaLookupItem = {
      val renamed = renamesMap.get(element.name).collect {
        case (`element`, name) => name
      }

      new ScalaResolveResult(element, renamed = renamed).getLookupElement(
        isClassName = true,
        isInImport = isInImport,
        isInStableCodeReference = isInStableCodeReference,
        isInSimpleString = isInSimpleString
      ).get
    }

    private[this] def isValidAndAccessible(member: PsiMember): Boolean =
      member.isValid &&
        isAccessible(member, invocationCount)(place)

    private[this] def isApplicable(clazz: PsiClass): Boolean = clazz match {
      case _: ScClass |
           _: ScTrait => isInImport || classesOnly
      case _: ScObject => isInImport || !classesOnly
      case _ => true
    }
  }

  private[this] object CompletionState {

    def apply(place: PsiElement, isInSimpleString: Boolean)
             (implicit parameters: CompletionParameters): CompletionState = {
      val (isInStableCodeReference, classesOnly) = getContextOfType(place, false, classOf[ScStableCodeReference]) match {
        case null => (false, false)
        case codeReference => (true, !codeReference.getContext.isInstanceOf[ScConstructorPattern])
      }

      CompletionState(
        place,
        parameters.getInvocationCount,
        isInSimpleString,
        isInImport(place),
        isInStableCodeReference,
        classesOnly,
        annotationsOnly(place)
      )
    }
  }

  private def completeClassName(result: CompletionResultSet)
                               (implicit parameters: CompletionParameters,
                                context: ProcessingContext): Unit =
    positionFromParameters match {
      case dummyPosition if shouldRunClassNameCompletion(dummyPosition, result.getPrefixMatcher) =>
        completeClassName(dummyPosition, result)
      case _ =>
    }

  private[completion] def completeClassName(dummyPosition: PsiElement, result: CompletionResultSet)
                                           (implicit parameters: CompletionParameters,
                                            context: ProcessingContext): Boolean = {
    val isInSimpleString = dummyPosition.getNode.getElementType match {
      case `tSTRING` | `tMULTILINE_STRING` => true
      case _ => false
    }

    val position = if (isInSimpleString) positionInString(dummyPosition) else dummyPosition
    if (!isInScalaContext(position, isInSimpleString)) return true

    implicit val project: Project = position.getProject
    implicit val state: CompletionState = CompletionState(position, isInSimpleString)
    val maybeExpectedTypes = expectedTypeAfterNew(dummyPosition)

    import collection.JavaConverters._

    val QualNameToType = StdTypes.instance.QualNameToType
    val syntheticLookupElements = for {
      clazz <- SyntheticClasses.get(project).all.values
      if !QualNameToType.contains(clazz.qualifiedName)

      if state.isValidClass(clazz)
    } yield state.createLookupItem(clazz, maybeExpectedTypes)

    result.addAllElements(syntheticLookupElements.asJava)

    val prefixMatcher = result.getPrefixMatcher
    AllClassesGetter.processJavaClasses(
      if (state.annotationsOnly) parameters.withInvocationCount(2) else parameters,
      prefixMatcher,
      parameters.getInvocationCount <= 1,
      new Consumer[PsiClass] {
        override def consume(`class`: PsiClass): Unit = `class` match {
          case _: PsiClassWrapper =>
          case _ =>
            //todo: filter according to position
            for {
              clazz <- `class` :: getCompanionModule(`class`).toList
              if state.isValidClass(clazz)

              lookupElement = state.createLookupItem(clazz, maybeExpectedTypes)
            } result.addElement(lookupElement)
        }
      }
    )

    if (!state.annotationsOnly) {
      val manager = ScalaPsiManager.instance
      val lookupElements = for {
        name <- manager.getStableTypeAliasesNames
        if prefixMatcher.prefixMatches(name)

        alias <- manager.getStableAliasesByName(name, position.resolveScope)
        if state.isValidAlias(alias)
      } yield state.createLookupItem(alias)

      result.addAllElements(lookupElements.asJava)
    }

    val lookupElements = for {
      (element, name) <- state.renamesMap.values
      if prefixMatcher.prefixMatches(name)
      if !prefixMatcher.prefixMatches(element.name)

      lookupItem = element match {
        case clazz: PsiClass if state.isValidClass(clazz) => state.createLookupItem(clazz, maybeExpectedTypes)
        case alias: ScTypeAlias if state.isValidAlias(alias) => state.createLookupItem(alias)
        case _ => null
      }
      if lookupItem != null
    } yield lookupItem

    result.addAllElements(lookupElements.asJava)

    if (isInSimpleString) result.stopHere()
    false
  }

  private[this] def positionInString(place: PsiElement)
                                    (implicit parameters: CompletionParameters) =
    ScalaPsiElementFactory.createExpressionFromText(
      "s" + place.getText,
      place.getContext.getContext
    ).findElementAt(
      parameters.getOffset - parameters.getPosition.getTextRange.getStartOffset + 1
    )
}
