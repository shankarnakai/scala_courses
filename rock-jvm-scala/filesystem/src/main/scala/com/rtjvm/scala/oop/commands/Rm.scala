package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(path: String) extends Command {
  override def apply(state: State): State = {
   val wd = state.wd
    val absolutePath =
      if (path.startsWith(Directory.SEPARATOR)) path
      else if(wd.isRoot) wd.path + path
      else wd.path + Directory.SEPARATOR + path

    if(Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("It's not allowed delete the root folder")
    else
      doRm(state, absolutePath)
  }

  def doRm(state: State, path: String) : State = {
    def rmHelper(current: Directory, path: List[String]) : Directory = {
      if(path.isEmpty) current
      else if(path.tail.isEmpty) current.removeEntry(path.head)
      else {
        val next = current.findEntry(path.head)
        if(!next.isDirectory) current
        else {
          val newNext = rmHelper(next.asDirectory, path.tail)
          if(newNext == next) current
          else current.replaceEntry(path.head, newNext)
        }
      }
    }

    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot = rmHelper(state.root, tokens)
    if(newRoot == state.root) state.setMessage(path + ": no such file or directory")
    else State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }
}
