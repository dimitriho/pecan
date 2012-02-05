package fr.idho.pecan

import parser.PhpParser

object Analyzer extends App {
  val parser = new PhpParser
  for (file <- args) { 
    val res = parser.parse(scala.io.Source.fromFile(file).mkString)
    println(res)
  }
}
