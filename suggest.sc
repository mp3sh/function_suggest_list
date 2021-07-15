import scala.annotation.tailrec

class SuggestService(companyNames : Seq[String]) {
  case class Node(label: Map[Char, String], children: Map[Char, Node])

  def apply(companyNames: Seq[String]): Node = {

    val root = Node(Map.empty[Char, String], Map.empty[Char, Node])

    def make_node(node: Node, word: String = "$"): Node = {

      if (word == "$") node else {

        if (node.label.contains(word(0))){
          val words = if (node.label(word(0)).length > word.length) {
            (node.label(word(0)), word)
          } else {
            (word, node.label(word(0)))
          }

          val left = words._1.foldLeft(""){(acc, char) =>
            if (words._2.startsWith(acc + char)){
              acc + char
            }else{
              acc
            }
          }

          val right = words._1.replace(left, "")

          val children_label =
            if (node.children.contains(words._1(0))) {
              node.children(words._1(0)).label
            } else {
              Map.empty[Char,String]
            }

          val children_children =
            if (node.children.contains(words._1(0))) {
              node.children(words._1(0)).children
            } else {
              Map.empty[Char,Node]
            }

          if (node.label(word(0)).length > word.length){
            make_node(
              Node(
                node.label ++ Map(left(0) -> left),
                node.children ++ Map(words._2(0) ->
                  make_node(
                    Node(
                      Map.empty,
                      Map(right(0) ->
                        Node(children_label, children_children))),right))))

          } else {
            make_node(
              Node(
                node.label ++ Map(words._1(0) -> left),
                node.children ++ Map(words._2(0) ->
                  make_node(
                    Node(
                      children_label,
                      children_children),right))))

          }

        } else {
          make_node(
            Node(
              node.label ++ Map(word(0) -> word),
              node.children))

        }
      }
    }

    @tailrec
    def make_trie(companyNames: Seq[String], root: Node, i: Int = 0): Node = {
      if (i == companyNames.length) root else {
        make_trie(companyNames, make_node(root, companyNames(i)), i+1)
      }
    }

    make_trie(companyNames, root)
  }

  private def flattenTree(node: Node, word: String): Seq[String] = {
    node.label.foldLeft(Seq[String]())((acc, x) =>
      if (node.children.isDefinedAt(x._1)) {
        if (node.children(x._1).label.size > 1) {
          ((word + x._2) +: flattenTree(node.children(x._1), word + x._2)) ++ acc
        } else {
          acc ++ ((word + x._2) +: flattenTree(node.children(x._1), word + x._2))
        }
      } else {
        Seq(word + x._2) ++ acc
      }
    )
  }

  @scala.annotation.tailrec
  private def getNodes(node: Node, word: String, acc: String = "", i: Int = 0): Option[Seq[String]] = {
    if (acc == word) {
      Some(flattenTree(node, word))
    } else {
      node.label.filter(_._1 == word(i)) match {
        case l: Map[Char, String] =>

          val k = l.keys.head
          val v = node.label(k)
          val d = acc + v

          val w = if (d.length > word.length) {
            (d, word)
          } else {
            (word, d)
          }

          val pattern = w._2.r
          pattern.findFirstIn(w._1) match {

            case Some(u) =>
              if (u == word) {
                node.children.find(_._1 == k) match {
                  case Some(_) =>
                    val node_1 = Node(node.label.filter(_._1 == k),node.children).children(word(i))
                    if (d == word) {Some(flattenTree(node_1, d))
                    } else {
                      Some(d +: flattenTree(node_1, d))
                    }
                  case _ =>
                    if (d == word) {None} else {
                      val node_2 = Node(node.label.filter(_._1 == k),Map.empty)
                      Some(flattenTree(node_2, d))
                    }
                }
              } else {
                node.children.find(_._1 == k) match {
                  case Some(_) => getNodes(node.children(k), word, acc + v, i + v.length)
                  case _ => None
                }
              }
            case _ => None

          }
        case _ => None
      }
    }
  }

  def suggest(input: String, numberOfSuggest : Int) : Seq[String] = {

    getNodes(this(companyNames),input).get.take(numberOfSuggest)

  }

}
