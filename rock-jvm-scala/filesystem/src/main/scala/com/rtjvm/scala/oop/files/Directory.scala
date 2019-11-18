package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FileSystemException

import scala.annotation.tailrec

class Directory( override val parentPath: String,
                 override val name: String,
                 val contents: List[DirEntry]) extends DirEntry(parentPath, name) {

  def getAllFoldersInPath: List[String] = path.substring(1)
    .split(Directory.Separator)
    .toList
    .filter(x => !x.isEmpty)

  def findEntry(entryName: String): DirEntry = {
    //TODO: Search for more explanation about @tailrec annotation
    @tailrec
    def findEntryHelper(name: String, contentList: List[DirEntry]) : DirEntry =
      if(contentList.isEmpty) null
      else findEntryHelper(name, contentList.tail)


    findEntryHelper(name, contents)
  }

  def addEntry(newEntry: DirEntry) : Directory = new Directory(parentPath, name, contents:+newEntry)

  def replaceEntry(entryName: String, newEntry: Directory): Directory =
    new Directory(parentPath, name, contents.filter(e => !e.name.equals(entryName)) :+ newEntry)


  def findDescendant(path: List[String]): Directory =
    if(path.isEmpty) this
    else findEntry(path.head)
      .asDirectory.findDescendant(path.tail)

  def hasEntry(name: String): Boolean = findEntry(name)  != null

  override def getType: String = "Directory"

  override def asDirectory: Directory = this

  override def asFile: File = throw new FileSystemException("A directory can't be converted to a File")

}

object Directory {
  val Separator = "/"
  val ROOT_PATH= "/"

  def ROOT: Directory = Directory.empty("", "")
  def empty(parentPath: String, name: String) = new Directory(parentPath, name, List())

}
