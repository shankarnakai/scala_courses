package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {

  override def apply(state: State) : State  = {
    val wd = state.wd
    if(wd.hasEntry(name)) {
     state.setMessage(s"Entry ${name} already existe!")
    } else if(name.contains(Directory.Separator)) {
      state.setMessage(s"${name} must not contain separators!")
    } else if(checkIllegal(name)) {
     state.setMessage(s"${name}: Ileggal entry name!")
    } else {
      doCreateEntry(state, name)
    }
  }

  def checkIllegal(name: String) : Boolean = name.contains(".")

  def doCreateEntry(state : State, name: String) : State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if(path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head)
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry.asDirectory, path.tail, newEntry))
      }
    }

    val wd = state.wd
    val allDirsInPath = wd.getAllFoldersInPath;

    val newEntry = createSpecificEntry(state)
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def createSpecificEntry(state : State): DirEntry
}
