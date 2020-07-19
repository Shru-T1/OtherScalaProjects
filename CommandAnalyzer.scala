object CommandParser extends App {

  case class GameObj(desc: String, kind: String)

  val grammar = List(
    "look",
    "inventory",
    "go {direction}",
    "climb {object}",
    "take {object}",
    "drop {object}",
    "examine {object}",
    "search {object}",
    "sit on {object}",
    "lie on {object}",
    "open {container}",
    "close {container}",
    "lock {container}",
    "unlock {container}",
    "put {item} in {container}",
    "put {item} on {supporter}",
    "wear {clothing}",
    "take off {clothing}",
    "tie {item} to {object}",
    "talk to {person}")

  val world = List(
    GameObj("north", "direction"),
    GameObj("south", "direction"),
    GameObj("east", "direction"),
    GameObj("west", "direction"),
    GameObj("comfy chair", "object"),
    GameObj("shabby twin bed", "object"),
    GameObj("elegant carpet", "object"),
    GameObj("soccer ball", "item"),
    GameObj("beach ball", "item"),
    GameObj("small green frog", "item"),
    GameObj("small tree frog", "item"),
    GameObj("large wooden box", "container"),
    GameObj("flimsy cardboard box", "container"),
    GameObj("solid wooden table", "supporter"),
    GameObj("glass side stand", "supporter"),
    GameObj("purple hoodie", "clothing"),
    GameObj("leather jacket", "clothing"),
    GameObj("very old man", "person"),
    GameObj("very young woman", "person"))

  // ==========================================================
  // helper functions
  // ==========================================================

  // turn a string of words separated by spaces into a list of those words
  def getWordList(words: String): List[String] =
  {return words.split(" ").toList}


  // ==========================================================
  // functions on world
  // ==========================================================

  // Note: All lists of string that are returned from the following methods should be distinct (no duplicates) and they
  // should be in sorted order.

  // returns a list of all the adjectives (all but last words in desc) in world
  // example: cardboard, flimsy, very [ but not 'box' ]
  def getAdjectives: List[String] = {
    world.flatMap(g => getWordList(g.desc).init).distinct.sorted
  }


  // returns a list of nouns (last words in desc) from world
  // example: box, frog, man
  def getNouns: List[String] = {
    world.flatMap(g => getWordList(g.desc).lastOption).distinct.sorted
  }

  // returns a list of game-object kinds from world
  // example: direction, item, object
  def getGameObjectKinds: List[String] = {
    world.flatMap(g => getWordList(g.kind)).distinct.sorted
  }

  // returns a list of game-objects associated with a particular noun (no need to be sorted)
  // example: getGameObjects("frog") =>
  //   List(GameObj("small green frog", "item"), GameObj("small tree frog", "item"))
  def getGameObjects(noun: String): List[GameObj] = {
    world.filter(g=>g.desc.contains(noun))
  }

  // ==========================================================
  // functions on grammar
  // ==========================================================

  // Note: All lists of string that are returned from the following methods should be distinct (no duplicates) and they
  // should be in sorted order.

  // returns a list of all verbs (first words) in grammar
  // example: look, examine, put
  def getVerbs: List[String] = {
    grammar.flatMap(g => getWordList(g).headOption).distinct.sorted
  }

  // returns a list of all prepositions in grammar
  // prepositions are words in grammar strings that are not verbs and not in curly braces
  // example: in, on, to
  def getPrepositions: List[String] = {
    grammar.flatMap(g => getWordList(g)).distinct.diff(getVerbs).filterNot(_.contains("{")).distinct.sorted
  }


  //var str = getActions//test segment
  //println(str.toString)
  // returns a list of all actions from grammar
  // actions are formed by combining the verb with its preposition (if any) with an underscore between them
  // example: look, examine, put_in
  def getActions: List[String] = {
    grammar.map(g => getWordList(g).filterNot(_.contains("{")).mkString("_"))
  }

  // returns a list of game-object kinds from grammar
  // kinds are all the words in curly braces
  // example: direction, item, object
  def getGrammarObjectKinds: List[String] = {
    grammar.flatMap(g => getWordList(g)).filter(_.contains("{")).distinct.sorted
  }

  // given a verb, returns a list of all grammar strings associated with it
  // example: getGrammarStrings("put") =>
  //   List("put {item} in {object}", "put {item} on {object}")
  def getGrammarStrings(verb: String): List[String] = {
    grammar.filter(g=>g.contains(verb))
  }

  // ==========================================================
  // getVocab function
  // ==========================================================

  // returns a list of all known words (from grammar and world)
  // note: does not include words in curly braces: {object} {direction} etc.
  def getVocab: List[String] = {
    getAdjectives ++ getNouns ++ getGameObjectKinds ++ getVerbs ++ getPrepositions
  }

  // ==========================================================
  // command related function
  // ==========================================================

  // return true if the words contain a preposition
  def hasPrep(words: String): Boolean = {
    !(getWordList(words) intersect getPrepositions).isEmpty
  }


  // return true if the words match the specified game object
  // for a string of words to match a game object, all the words in the string
  // must be words contained in the description of the game object
  // example: wordsMatchGameObj("tree tree", GameObj("small tree frog", "item") ==> true
  // example: wordsMatchGameObj("small ball", GameObj("beach ball", "item") ==> false
  def wordsMatchGameObj(words: String, gameObj: GameObj): Boolean = {
    ((getWordList(words).distinct) diff (getWordList(gameObj.desc) ++ getWordList(gameObj.kind) )).isEmpty
  }

  // return true if the words (cmdWords) match the specified grammar (pattern).
  // Take "put {item} in {container}" as an example pattern. The first word in cmdWords must be "put". The following
  // word(s) must match the desc of an "item" kind GameObj based on wordsMatchGameObj function. The next word must be
  // "in". The ending word(s) must match the desc of a "container" kind GameObj based on wordsMatchGameObj function
 def wordsMatchPattern(cmdWords: String, pattern: String): Boolean = checkIfMatch(getWordList(cmdWords),getWordList(pattern))

  def checkIfMatch(cmdWords: List[String], pattern: List[String]):Boolean = cmdWords match {
    case Nil | "" :: Nil => if (pattern == Nil) {
      true
    } else false
    case x :: Nil => if (pattern.head.contains("{") && pattern.tail == Nil) {
      true
    } else if (cmdWords == pattern) true else false
    case y :: ys => if (pattern.head.contains("{") && pattern.tail == Nil)
      true else if (pattern == Nil) false
    else if (pattern.head.contains("{") && (ys.head != pattern.tail.head)) {
      checkIfMatch(ys, pattern)
    }
      else if (pattern.head.contains("{") && (ys.head == pattern.tail.head)) {
        checkIfMatch(ys.tail, pattern.tail.tail)
      }
      else if (y == pattern.head) {
        checkIfMatch(ys, pattern.tail)
      }
      else false
    }

}