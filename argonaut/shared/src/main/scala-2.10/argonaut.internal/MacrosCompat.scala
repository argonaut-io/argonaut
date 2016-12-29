package argonaut.internal

trait MacrosCompat {
  type Context = scala.reflect.macros.Context

  def getDeclarations(c: Context)(tpe: c.universe.Type): c.universe.MemberScope = tpe.declarations

  def getParameterLists(c: Context)(method: c.universe.MethodSymbol): List[List[c.universe.Symbol]] = method.paramss

  def getDeclaration(c: Context)(tpe: c.universe.Type, name: c.universe.Name): c.universe.Symbol = tpe.declaration(name)

  def createTermName(c: Context)(name: String): c.universe.TermName = c.universe.newTermName(name)
}
