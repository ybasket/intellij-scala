package org.jetbrains.plugins.scala.compiler.data.serialization

import org.jetbrains.plugins.scala.compiler.data.serialization.ArgListSerializer._
import org.jetbrains.plugins.scala.compiler.data.serialization.extensions._
import org.jetbrains.plugins.scala.compiler.data.worksheet.{WorksheetArgs, WorksheetArgsPlain, WorksheetArgsRepl}

/** TODO: cover with property-based tests */
object WorksheetArgsSerializer extends ArgListSerializer[WorksheetArgs] {

  private val ReplMode  = "repl"
  private val PlainMode = "plain"

  override def serialize(value: WorksheetArgs): ArgList = value match {
    case plain: WorksheetArgsPlain => PlainMode +: WorksheetArgsPlainSerializer.serialize(plain)
    case repl: WorksheetArgsRepl   => ReplMode +: WorksheetArgsReplSerializer.serialize(repl)
  }

  override def deserialize(args: ArgList): Either[DeserializationError, WorksheetArgs] =
    args match {
      case Seq()                    => error("Args are empty")
      case Seq(PlainMode, other@_*) => WorksheetArgsPlainSerializer.deserialize(other)
      case Seq(ReplMode, other@_*)  => WorksheetArgsReplSerializer.deserialize(other)
      case Seq(unknown, other@_*)   => error(s"Unknown worksheet run mode: $unknown")
    }
}

object WorksheetArgsPlainSerializer extends ArgListSerializer[WorksheetArgsPlain] {

  import SerializationUtils._

  override def serialize(value: WorksheetArgsPlain): ArgList = Seq(
    value.worksheetClassName,
    fileToPath(value.pathToRunnersJar),
    fileToPath(value.worksheetTempFile),
    value.originalFileName,
    filesToPaths(value.outputDirs)
  )

  override def deserialize(args: ArgList): Either[DeserializationError, WorksheetArgsPlain] =
    for {
      worksheetClassName <- Right(args.head)
      pathToRunners      <- pathToFile(args(1), "pathToRunners").lift
      worksheetTempFile  <- pathToFile(args(2), "worksheetTempFile").lift
      originalFileName   <- notNull(args(3), "originalFileName").lift
      outputDirs         <- args.drop(4).map(pathToFile(_, "outputDirs")).collectErrors
    } yield WorksheetArgsPlain(
      worksheetClassName,
      pathToRunners,
      worksheetTempFile,
      originalFileName,
      outputDirs
    )
}

object WorksheetArgsReplSerializer extends ArgListSerializer[WorksheetArgsRepl] {

  import SerializationUtils._

  override def serialize(value: WorksheetArgsRepl): ArgList = Seq(
    value.sessionId,
    value.codeChunk,
    SerializationUtils.filesToPaths(value.outputDirs)
  )

  override def deserialize(args: ArgList): Either[DeserializationError, WorksheetArgsRepl] =
    for {
      sessionId  <- notNull(args.head, "repl session id").lift
      codeChunk  <- notNull(args(1), "codeChunk").lift
      outputDirs <- args.drop(2).map(SerializationUtils.pathToFile(_, "outputDirs")).collectErrors
    } yield WorksheetArgsRepl(
      sessionId,
      codeChunk,
      outputDirs
    )
}