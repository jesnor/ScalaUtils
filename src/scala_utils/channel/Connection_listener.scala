package scala_utils.channel

trait Connection_listener [R, S] {
  def accept_connection () : Option [Channel [R, S]]
}
