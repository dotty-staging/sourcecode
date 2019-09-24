package sourcecode

import scala.language.implicitConversions
import scala.quoted._
import scala.tasty.Reflection

trait NameMacros {
  inline implicit def generate: Name =
    ${ Macros.nameImpl }
}

trait NameMachineMacros {
  inline implicit def generate: Name.Machine =
    ${ Macros.nameMachineImpl }
}

trait FullNameMacros {
  inline implicit def generate: FullName =
    ${ Macros.fullNameImpl }
}

trait FullNameMachineMacros {
  inline implicit def generate: FullName.Machine =
    ${ Macros.fullNameMachineImpl }
}

trait FileMacros {
  inline implicit def generate: sourcecode.File =
    ${ Macros.fileImpl }
}

trait LineMacros {
  inline implicit def generate: sourcecode.Line =
    ${ Macros.lineImpl }
}

trait EnclosingMacros {
  inline implicit def generate: Enclosing =
    ${ Macros.enclosingImpl }
}

trait EnclosingMachineMacros {
  inline implicit def generate: Enclosing.Machine =
    ${ Macros.enclosingMachineImpl }
}

trait PkgMacros {
  inline implicit def generate: Pkg =
    ${ Macros.pkgImpl }
}

trait TextMacros {
  inline implicit def generate[T](v: => T): Text[T] = ${ Macros.text('v) }
  inline def apply[T](v: => T): Text[T] = ${ Macros.text('v) }
}

trait ArgsMacros {
  inline implicit def generate: Args =
    ${ Macros.argsImpl }
}

object Util{
  def isSynthetic(qctx: QuoteContext)(s: qctx.tasty.Symbol) = isSyntheticName(getName(qctx)(s))
  def isSyntheticName(name: String) = {
    name == "<init>" || (name.startsWith("<local ") && name.endsWith(">"))
  }
  def getName(qctx: QuoteContext)(s: qctx.tasty.Symbol) = {
    import qctx.tasty._
    s.name.trim
      .stripSuffix("$") // meh
  }
}

object Macros {

  def actualOwner(qctx: QuoteContext)(owner: qctx.tasty.Symbol): qctx.tasty.Symbol = {
    import qctx.tasty._
    var owner0 = owner
    // second condition is meh
    while(Util.isSynthetic(qctx)(owner0) || Util.getName(qctx)(owner0) == "ev") {
      owner0 = owner0.owner
    }
    owner0
  }

  def nameImpl(implicit qctx: QuoteContext): Expr[Name] = {
    import qctx.tasty._
    val owner = actualOwner(qctx)(rootContext.owner)
    val simpleName = Util.getName(qctx)(owner)
    '{Name(${Expr(simpleName)})}
  }

  private def adjustName(s: String): String =
    // Required to get the same name from dotty
    if (s.startsWith("<local ") && s.endsWith("$>"))
      s.stripSuffix("$>") + ">"
    else
      s

  def nameMachineImpl(implicit qctx: QuoteContext): Expr[Name.Machine] = {
    import qctx.tasty._
    val owner = rootContext.owner
    val simpleName = adjustName(Util.getName(qctx)(owner))
    '{Name.Machine(${Expr(simpleName)})}
  }

  def fullNameImpl(implicit qctx: QuoteContext): Expr[FullName] = {
    import qctx.tasty._
    val owner = actualOwner(qctx)(rootContext.owner)
    val fullName =
      owner.fullName.trim
        .split("\\.", -1)
        .filterNot(Util.isSyntheticName)
        .map(_.split("_\\$", -1).dropWhile(_.isEmpty).mkString.stripSuffix("$")) // meh
        .mkString(".")
    '{FullName(${Expr(fullName)})}
  }

  def fullNameMachineImpl(implicit qctx: QuoteContext): Expr[FullName.Machine] = {
    import qctx.tasty._
    val owner = rootContext.owner
    val fullName = owner.fullName.trim
      .split("\\.", -1)
      .map(_.stripPrefix("_$").stripSuffix("$")) // meh
      .map(adjustName)
      .mkString(".")
    '{FullName.Machine(${Expr(fullName)})}
  }

  def fileImpl(implicit qctx: QuoteContext): Expr[sourcecode.File] = {
    import qctx.tasty._
    val file = rootPosition.sourceFile.jpath.toAbsolutePath.toString
    '{sourcecode.File(${Expr(file)})}
  }

  def lineImpl(implicit qctx: QuoteContext): Expr[sourcecode.Line] = {
    import qctx.tasty._
    val line = rootPosition.startLine + 1
    '{sourcecode.Line(${Expr(line)})}
  }

  def enclosingImpl(implicit qctx: QuoteContext): Expr[Enclosing] = {
    val path = enclosing(qctx)(
      !Util.isSynthetic(qctx)(_)
    )

    '{Enclosing(${Expr(path)})}
  }

  def enclosingMachineImpl(implicit qctx: QuoteContext): Expr[Enclosing.Machine] = {
    val path = enclosing(qctx, machine = true)(_ => true)
    '{Enclosing.Machine(${Expr(path)})}
  }

  def pkgImpl(implicit qctx: QuoteContext): Expr[Pkg] = {
    import qctx.tasty._
    val path = enclosing(qctx) {
      case IsPackageDefSymbol(_) => true
      case _ => false
    }

    '{Pkg(${Expr(path)})}
  }

  def argsImpl(implicit qctx: QuoteContext): Expr[Args] = {
    import qctx.tasty._

    val param: List[List[ValDef]] = {
      def nearestEnclosingMethod(owner: Symbol): List[List[ValDef]] =
        owner match {
          case IsDefDefSymbol(defSym) =>
            val IsDefDef(ddef) = defSym.tree
            ddef.paramss
          case IsClassDefSymbol(classSym) =>
            val IsClassDef(cdef) = classSym.tree
            cdef.constructor.paramss
          case _ =>
            nearestEnclosingMethod(owner.owner)
        }

      nearestEnclosingMethod(rootContext.owner)
    }

    val texts0 = param.map(_.foldRight('{List.empty[Text[_]]}) {
      case (vd @ ValDef(nme, _, optV), l) =>
        '{Text(${optV.fold('None)(_.seal)}, ${Expr(nme)}) :: $l}
    })
    val texts = texts0.foldRight('{List.empty[List[Text[_]]]}) {
      case (l, acc) =>
        '{$l :: $acc}
    }

    '{Args($texts)}
  }


  def text[T: Type](v: Expr[T])(implicit qctx: QuoteContext): Expr[sourcecode.Text[T]] = {
    import qctx.tasty._
    val txt = v.unseal.pos.sourceCode
    '{sourcecode.Text[T]($v, ${Expr(txt)})}
  }

  sealed trait Chunk
  object Chunk{
    case class PkgObj(name: String) extends Chunk
    case class ClsTrt(name: String) extends Chunk
    case class ValVarLzyDef(name: String) extends Chunk

  }

  def enclosing(qctx: QuoteContext, machine: Boolean = false)(filter: qctx.tasty.Symbol => Boolean): String = {
    import qctx.tasty._

    var current = rootContext.owner
    if (!machine)
      current = actualOwner(qctx)(current)
    var path = List.empty[Chunk]
    while(current.toString != "NoSymbol" && current != defn.RootPackage && current != defn.RootClass){
      if (filter(current)) {

        val chunk = current match {
          case IsValDefSymbol(_) => Chunk.ValVarLzyDef
          case IsDefDefSymbol(_) => Chunk.ValVarLzyDef
          case _ => Chunk.PkgObj
        }

        // TODO
        // val chunk = current match {
        //   case x if x.flags.isPackage => Chunk.PkgObj
        //   case x if x.flags.isModuleClass => Chunk.PkgObj
        //   case x if x.flags.isClass && x.asClass.isTrait => Chunk.ClsTrt
        //   case x if x.flags.isClass => Chunk.ClsTrt
        //   case x if x.flags.isMethod => Chunk.ValVarLzyDef
        //   case x if x.flags.isTerm && x.asTerm.isVar => Chunk.ValVarLzyDef
        //   case x if x.flags.isTerm && x.asTerm.isLazy => Chunk.ValVarLzyDef
        //   case x if x.flags.isTerm && x.asTerm.isVal => Chunk.ValVarLzyDef
        // }
        //
        // path = chunk(Util.getName(qctx)(current)) :: path

        path = chunk(Util.getName(qctx)(current).stripSuffix("$")) :: path
      }
      current = current.owner
    }
    path.map{
      case Chunk.PkgObj(s) => adjustName(s) + "."
      case Chunk.ClsTrt(s) => adjustName(s) + "#"
      case Chunk.ValVarLzyDef(s) => adjustName(s) + " "
    }.mkString.dropRight(1)
  }
}
