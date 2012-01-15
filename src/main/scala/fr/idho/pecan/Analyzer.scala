package fr.idho.pecan

import parser.PhpParser

object Analyzer extends App {
  for (file <- args) {
    val res = PhpParser.parse(scala.io.Source.fromFile(file).mkString)
    println(res)
  }
}
