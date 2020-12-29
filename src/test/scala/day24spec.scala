 package day24

import util.InputLines
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day24Spec extends AnyFlatSpec with Matchers {
  val sample = List(
    "sesenwnenenewseeswwswswwnenewsewsw",
    "neeenesenwnwwswnenewnwwsewnenwseswesw",
    "seswneswswsenwwnwse",
    "nwnwneseeswswnenewneswwnewseswneseene",
    "swweswneswnenwsewnwneneseenw",
    "eesenwseswswnenwswnwnwsewwnwsene",
    "sewnenenenesenwsewnenwwwse",
    "wenwwweseeeweswwwnwwe",
    "wsweesenenewnwwnwsenewsenwwsesesenwne",
    "neeswseenwwswnwswswnw",
    "nenwswwsewswnenenewsenwsenwnesesenew",
    "enewnwewneswsewnwswenweswnenwsenwsw",
    "sweneswneswneneenwnewenewwneswswnese",
    "swwesenesewenwneswnwwneseswwne",
    "enesenwswwswneneswsenwnewswseenwsese",
    "wnwnesenesenenwwnenwsewesewsesesew",
    "nenewswnwewswnenesenwnesewesw",
    "eneswnwswnwsenenwnwnwwseeswneewsenese",
    "neswnwewnwnwseenwseesewsenwsweewe",
    "wseweeenwnesenwwwswnew"
  )

  "Part 1" should "work for sample" in {
    val tw = new TileWalker()
    sample.foreach(tw.flip _)
    tw.blackTiles shouldEqual 10
  }

  "Part 1" should "work for real input" in {
    val tw = new TileWalker()
    InputLines.forDay(24).foreach(tw.flip _)
    tw.blackTiles shouldEqual 495
  }

  "Part 2" should "work for sample" in {
    val tw = new TileWalker()
    sample.foreach(tw.flip _)
    var tiles = tw.tiles

    tiles = TileWalker.tileArtDay(tiles)
    tiles.size shouldEqual 15
    tiles = TileWalker.tileArtDay(tiles)
    tiles.size shouldEqual 12
    tiles = TileWalker.tileArtDay(tiles)
    tiles.size shouldEqual 25
    tiles = TileWalker.tileArtDay(tiles)
    tiles.size shouldEqual 14

    for (i <- 4 until 100) {
      tiles = TileWalker.tileArtDay(tiles)
    }

    tiles.size shouldEqual 2208
  }

  "Part 2" should "work for real" in {
    val tw = new TileWalker()
    InputLines.forDay(24).foreach(tw.flip _)
    var tiles = tw.tiles

    for (i <- 0 until 100) {
      tiles = TileWalker.tileArtDay(tiles)
    }

    tiles.size shouldEqual 4012
  }
}
