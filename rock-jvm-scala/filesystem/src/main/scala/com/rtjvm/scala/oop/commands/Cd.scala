package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String ) extends Command {

  override def apply(state: State): State = {
    val root = state.root
    val wd = state.wd

    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if(wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    val destinationDirectory = doFindEntry(root, absolutePath)

    if(destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory ")
    else State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(current: Directory, path: List[String]): DirEntry =
      if(path.isEmpty || path.head.isEmpty) current
      else if(path.tail.isEmpty) current.findEntry(path.head)
      else {
       val next = current.findEntry(path.head)
        if (next == null  || !next.isDirectory) null
        else findEntryHelper(next.asDirectory, path.tail)
      }

    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    findEntryHelper(root, tokens)
  }
}
