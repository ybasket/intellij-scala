package org.jetbrains.plugins.scala.annotator.element

import com.intellij.openapi.util.TextRange
import org.jetbrains.plugins.scala.annotator.ScalaAnnotationHolder
import org.jetbrains.plugins.scala.annotator.element.ScExpressionAnnotator.checkExpressionType
import org.jetbrains.plugins.scala.annotator.element.ScTypedExpressionAnnotator.mismatchRangesIn
import org.jetbrains.plugins.scala.annotator.quickfix.ReportHighlightingErrorQuickFix
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlockExpr, ScFunctionExpr}
import org.jetbrains.plugins.scala.lang.psi.types.api.FunctionType
import org.jetbrains.plugins.scala.lang.psi.types.api.FunctionType.isFunctionType

object ScFunctionExprAnnotator extends ElementAnnotator[ScFunctionExpr] {

  override def annotate(literal: ScFunctionExpr, typeAware: Boolean)(implicit holder: ScalaAnnotationHolder): Unit = {

    var problemWithParameters = false

    val (result, parameters) = (literal.result, literal.parameters)

    literal.expectedType() match {
      case Some(FunctionType(_, expectedParameterTypes)) =>

        // Missing parameters
        if (parameters.length < expectedParameterTypes.length) {
          val startElement = if (parameters.isEmpty) literal.leftParen.getOrElse(literal.params) else parameters.last
          val errorRange = startElement.nextVisualSiblingNotWhitespace match {
            case Some(nextElement) => new TextRange(startElement.getTextRange.getEndOffset - 1, nextElement.getTextOffset + 1)
            case None => startElement.getTextRange
          }
          val message = (if (expectedParameterTypes.length - parameters.length == 1) "Missing parameter: " else "Missing parameters: ") +
            expectedParameterTypes.drop(parameters.length).map(_.presentableText(literal)).mkString(", ")
          holder.createErrorAnnotation(errorRange, message)
          problemWithParameters = true
        }

        // Too many parameters
        if (!problemWithParameters && parameters.length > expectedParameterTypes.length) {
          if (!literal.hasParentheses) {
            holder.createErrorAnnotation(parameters.head, "Too many parameters")
          } else {
            val firstExcessiveParameter = parameters(expectedParameterTypes.length)
            val range = new TextRange(
              firstExcessiveParameter.prevVisualSiblingNotWhitespace.getOrElse(literal.params).getTextRange.getEndOffset - 1,
              firstExcessiveParameter.getTextOffset + 1)
            holder.createErrorAnnotation(range, "Too many parameters")
          }
          problemWithParameters = true
        }

        // Parameter type mismatch
        parameters.iterator.takeWhile(_ => !problemWithParameters).foreach { parameter =>
          (parameter.expectedParamType, parameter.typeElement.flatMap(_.`type`().toOption)) match {
            case (Some(expectedType), Some(annotatedType)) if !annotatedType.conforms(expectedType) =>
              val message = s"Type mismatch, expected: ${expectedType.presentableText(parameter)}, actual: ${parameter.typeElement.get.getText}"
              val ranges = mismatchRangesIn(parameter.typeElement.get, expectedType)(parameter)
              ranges.foreach { range =>
                val annotation = holder.createErrorAnnotation(range, message)
                annotation.registerFix(ReportHighlightingErrorQuickFix)
              }
              problemWithParameters = true
            case _ =>
          }
        }

      case _ =>
    }

    // Missing parameter type
    parameters.iterator.takeWhile(_ => !problemWithParameters).foreach { parameter =>
      if (parameter.typeElement.isEmpty && parameter.expectedParamType.isEmpty) {
        holder.createErrorAnnotation(parameter, "Missing parameter type")
        problemWithParameters = true
      }
    }

    // Result type mismatch
    if (!problemWithParameters) {
      val inMultilineBlock = literal match {
        case Parent(b: ScBlockExpr) => b.textContains('\n')
        case _ => false
      }

      if (!inMultilineBlock && literal.expectedType().exists(isFunctionType)) {
        result.foreach(checkExpressionType(_, typeAware, fromFunctionLiteral = true))
      } else {
        checkExpressionType(literal, typeAware, fromFunctionLiteral = true)
      }
    }
  }
}