package org.jetbrains.plugins.scala
package codeInsight.intention.types

import com.intellij.codeInsight.completion.{InsertHandler, InsertionContext}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.codeInsight.template._
import com.intellij.codeInsight.template.impl.TemplateManagerImpl
import com.intellij.openapi.editor.Editor
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageUtil
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.TypeAdjuster
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScBindingPattern, ScTypedPattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScFunctionExpr
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScParameterClause}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.result.TypingContext
import org.jetbrains.plugins.scala.lang.psi.types.{ScCompoundType, ScFunctionType, ScType, ScTypeText}

/**
 * Pavel.Fatin, 28.04.2010
 */

object AddOrRemoveStrategy extends UpdateStrategy

object AddOnlyStrategy extends UpdateStrategy {
  override def removeFromFunction(function: ScFunctionDefinition): Unit = {}
  override def removeFromParameter(param: ScParameter): Unit = {}
  override def removeFromPattern(pattern: ScTypedPattern): Unit = {}
  override def removeFromValue(value: ScPatternDefinition): Unit = {}
  override def removeFromVariable(variable: ScVariableDefinition): Unit = {}
}

abstract class UpdateStrategy extends Strategy {
  def addToFunction(function: ScFunctionDefinition, editor: Option[Editor]) {
    function.returnType.foreach {
      addTypeAnnotation(_, function, function.paramClauses, editor)
    }
  }

  def removeFromFunction(function: ScFunctionDefinition) {
    function.returnTypeElement.foreach(removeTypeAnnotation)
  }

  def addToValue(value: ScPatternDefinition, editor: Option[Editor]) {
    value.getType(TypingContext.empty).toOption.foreach {
      addTypeAnnotation(_, value, value.pList, editor)
    }
  }

  def removeFromValue(value: ScPatternDefinition) {
    value.typeElement.foreach(removeTypeAnnotation)
  }

  def addToVariable(variable: ScVariableDefinition, editor: Option[Editor]) {
    variable.getType(TypingContext.empty).toOption.foreach {
      addTypeAnnotation(_, variable, variable.pList, editor)
    }
  }

  def removeFromVariable(variable: ScVariableDefinition) {
    variable.typeElement.foreach(removeTypeAnnotation)
  }

  def addToPattern(pattern: ScBindingPattern, editor: Option[Editor]) {
    pattern.expectedType.foreach {
      addTypeAnnotation(_, pattern.getParent, pattern, None)
    }
  }

  def addToWildcardPattern(pattern: ScWildcardPattern) {
    pattern.expectedType.foreach {
      addTypeAnnotation(_, pattern.getParent, pattern, None)
    }
  }

  def removeFromPattern(pattern: ScTypedPattern) {
    val newPattern = ScalaPsiElementFactory.createPatternFromText(pattern.name, pattern.getManager)
    pattern.replace(newPattern)
  }

  def addToParameter(param: ScParameter, editor: Option[Editor]) {
    param.parentsInFile.findByType(classOf[ScFunctionExpr]) match {
      case Some(func) =>
        val index = func.parameters.indexOf(param)
        func.expectedType() match {
          case Some(ScFunctionType(_, params)) =>
            if (index >= 0 && index < params.length) {
              val paramExpectedType = params(index)
              val param1 = param.getParent match {
                case x: ScParameterClause if x.parameters.length == 1 =>
                  // ensure  that the parameter is wrapped in parentheses before we add the type annotation.
                  val clause: PsiElement = x.replace(ScalaPsiElementFactory.createClauseForFunctionExprFromText("(" + param.getText + ")", param.getManager))
                  clause.asInstanceOf[ScParameterClause].parameters.head
                case _ => param
              }
              addTypeAnnotation(paramExpectedType, param1.getParent, param1, None)
            }
          case _ =>
        }
      case _ =>
    }
  }

  def removeFromParameter(param: ScParameter) {
    val newParam = ScalaPsiElementFactory.createParameterFromText(param.name, param.getManager)
    val newClause = ScalaPsiElementFactory.createClauseForFunctionExprFromText(newParam.getText, param.getManager)
    val expr : ScFunctionExpr = PsiTreeUtil.getParentOfType(param, classOf[ScFunctionExpr], false)
    if (expr != null && expr.parameters.size == 1 &&
            (expr.params.clauses(0).getText.startsWith("(") && expr.params.clauses(0).getText.endsWith(")"))) {
      expr.params.clauses(0).replace(newClause)
    } else {
      param.replace(newParam)
    }
  }

  def addTypeAnnotation(t: ScType, context: PsiElement, anchor: PsiElement, editor: Option[Editor]) {
    def addActualType(annotation: ScTypeElement) = {
      val added = anchor.getParent.addAfter(annotation, anchor)
      val colon = ScalaPsiElementFactory.createColon(context.getManager)
      anchor.getParent.addAfter(colon, anchor)
      added
    }

    val tps: Seq[ScTypeElement] = t match {
      case ScCompoundType(comps, _, _) =>
        val uselessTypes = Set("_root_.scala.Product", "_root_.scala.Serializable", "_root_.java.lang.Object")
        comps.map(_.canonicalText).filterNot(uselessTypes.contains) match {
          case Seq(base) =>
            val te = ScalaPsiElementFactory.createTypeElementFromText(base, context.getManager)
            Seq(te)
          case types => (Seq(types.mkString(" with ")) ++ types).flatMap { t =>
            val te = ScalaPsiElementFactory.createTypeElementFromText(t, context.getManager)
            Seq(te)
          }
        }
      case someOrNone if Set("_root_.scala.Some", "_root_.scala.None").exists(someOrNone.canonicalText.startsWith) =>
        val tp = ScType.extractClassType(someOrNone, Option(context.getProject)) match {
          case Some((cl: ScTypeDefinition, sub)) => cl.superTypes.find(_.canonicalText.startsWith("_root_.scala.Option")) match {
            case Some(typ) => sub.subst(typ)
            case _ => someOrNone
          }
          case _ => someOrNone
        }
        Seq(ScalaPsiElementFactory.createTypeElementFromText(tp.canonicalText, context.getManager))
      case f => Seq(ScalaPsiElementFactory.createTypeElementFromText(f.canonicalText, context.getManager))
    }
    val added = addActualType(tps.head)

    editor match {
      case Some(e) if tps.size > 1 =>
        val project = context.getProject
        val expression = new Expression {
          val lookupItems: Array[LookupElement] = {
            val texts: Seq[ScTypeText] = tps.flatMap(_.getType().toOption).map(ScTypeText)
            texts.map { typeText =>
              val useCanonicalText: Boolean = texts.exists {
                s => s.tp.ne(typeText.tp) && s.presentableText == typeText.presentableText
              }
              val text =
                if (useCanonicalText) typeText.canonicalText.replace("_root_.", "")
                else typeText.presentableText
              LookupElementBuilder.create(typeText.tp, text).withInsertHandler(new InsertHandler[LookupElement] {
                override def handleInsert(context: InsertionContext, item: LookupElement): Unit = {
                  val topLevelEditor = InjectedLanguageUtil.getTopLevelEditor(context.getEditor)
                  val templateState = TemplateManagerImpl.getTemplateState(topLevelEditor)
                  if (templateState != null) {
                    val range = templateState.getCurrentVariableRange
                    if (range != null) {
                      //need to insert with FQNs
                      val newText = item.getObject.asInstanceOf[ScType].canonicalText
                      topLevelEditor.getDocument.replaceString(range.getStartOffset, range.getEndOffset, newText)
                    }
                  }
                }
              })
            }.toArray
          }

          val default = lookupItems.head

          override def calculateResult(context: ExpressionContext): Result = {
            new TextResult(default.getLookupString)
          }

          override def calculateLookupItems(context: ExpressionContext): Array[LookupElement] = {
            if (lookupItems.length > 1) lookupItems
            else null
          }

          override def calculateQuickResult(context: ExpressionContext): Result = calculateResult(context)
        }

        PsiDocumentManager.getInstance(project).commitAllDocuments()
        PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(e.getDocument)
        val builder: TemplateBuilderImpl = new TemplateBuilderImpl(added)
        builder.replaceElement(added, expression)
        e.getCaretModel.moveToOffset(added.getNode.getStartOffset)
        TemplateManager.getInstance(project).startTemplate(e, builder.buildInlineTemplate(), new TemplateEditingAdapter {
          override def templateFinished(template: Template, brokenOff: Boolean): Unit = {
            if (!brokenOff) {
              TypeAdjuster.markToAdjust(context)
            }
            super.templateFinished(template, brokenOff)
          }
        })
      case _ => TypeAdjuster.markToAdjust(added)
    }
  }

  def removeTypeAnnotation(e: PsiElement) {
    e.prevSiblings.find(_.getText == ":").foreach(_.delete())
    e.delete()
  }
}