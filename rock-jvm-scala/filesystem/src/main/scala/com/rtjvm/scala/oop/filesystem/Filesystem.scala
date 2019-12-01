package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.commands.Command
import com.rtjvm.scala.oop.files.Directory
/**
  * Created by Daniel on 28-Oct-17.
  */
object Filesystem extends App {
  val root = Directory.ROOT
  val initialState = State(root, root)

  initialState.show
  io.Source.stdin.getLines().foldLeft(initialState) {
    (state, input)  => {
      val newState = Command.from(input).apply(state)
      newState.show
      newState
    }
  }
}
