package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State =  {
    if(args.isEmpty) state
    else if(args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val filename = args(args.length - 1)
      val content = createContent(args, args.length - 2)

      if(">>".equals(operator))
        doEcho(state, content, filename, append = true)
      else if(">".equals(operator))
        doEcho(state, content, filename, append = false)
      else state.setMessage(createContent(args, args.length))
    }
  }

  private def createContent(args: Array[String], topIndex: Int) : String = {
    @tailrec
    def createContentHelper(currentIndex: Int, acc: String) : String =
      if(currentIndex >= topIndex) acc
      else createContentHelper(currentIndex + 1, acc + " " + args(currentIndex))
    createContentHelper(0, "")
  }

  private def getRootAfterEcho(current: Directory, path : List[String], contents: String, append: Boolean) : Directory =  {
    if(path.isEmpty) current
    else if(path.tail.isEmpty) {
      val dirEntry = current.findEntry(path.head)
      if(dirEntry == null) current.addEntry(new File(current.path, path.head, contents))
      else if(dirEntry.isDirectory) current
      else
        if(append) current.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
        else current.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    } else {
      val next = current.findEntry(path.head).asDirectory
      val newNext = getRootAfterEcho(next, path.tail, contents, append)

      if(newNext == next) current
      else current.replaceEntry(path.head, newNext)
    }
  }

  private def doEcho(state: State, contents: String, filename:String, append: Boolean) = {
    if(filename.contains(Directory.SEPARATOR))
      state.setMessage("Echo: filename must not contain separators")
    else {
     val newRoot : Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, contents, append)
      if(newRoot == state.root)
        state.setMessage(filename + ": no such file")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }

  }
}
