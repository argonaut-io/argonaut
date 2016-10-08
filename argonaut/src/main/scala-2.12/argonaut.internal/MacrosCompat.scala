package argonaut.internal

trait MacrosCompat {
  type Context = scala.reflect.macros.blackbox.Context

  def getDeclarations(c: Context)(tpe: c.universe.Type): c.universe.MemberScope = tpe.decls

  def getParameterLists(c: Context)(method: c.universe.MethodSymbol): List[List[c.universe.Symbol]] = method.paramLists

  def getDeclaration(c: Context)(tpe: c.universe.Type, name: c.universe.Name): c.universe.Symbol = tpe.decl(name)

  def createTermName(c: Context)(name: String): c.universe.TermName = c.universe.TermName(name)
}
