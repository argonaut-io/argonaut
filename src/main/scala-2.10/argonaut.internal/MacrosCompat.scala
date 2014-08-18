package argonaut.internal

trait MacrosCompat {
  import language.experimental.macros
  type Context = scala.reflect.macros.Context
}
