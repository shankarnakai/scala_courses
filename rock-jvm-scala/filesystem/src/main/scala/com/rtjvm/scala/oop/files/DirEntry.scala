package com.rtjvm.scala.oop.files

abstract class DirEntry(val parentPath: String, val name: String) {
  def path: String = parentPath + Directory.Separator + name

  def asDirectory: Directory
  def asFile: File

  def getType: String
}
