// Main.kt
// Testing ...

fun main() {
    val src = """
        record Pair { fst: int, snd: bool }

        relevant x = 42
        affine y = 10
        relevant z = not x
        relevant d = Pair { snd = true, fst = 42 }
        relevant l = [3, 4, "asd"]
        f(1, true)

        fun f(affine a: int, relevant b: bool) = 
            if y then not a + y + x else a + y + c

        Pair { fst = l + y, snd = false } + Pair { fst = 2, snd = true }
        [3, 4, 5] + ["asda"]
        record Point {coordinates: Pair, label: string}
        Point {coordinates = d, label = "p1"} + Pair { fst = 2, snd = true }
        Pointt {coordinates = d, label = "p1"} + Pointt {coordinates = d, label = "p1"}
        fun g (affine x: int, affine y: sdmlsfmkdl) = x
        fun t (affine x: int, affine y: list<int>) = t(1, [1])
        t(3, ["sdssa"])
        
    """.trimIndent()

    val tokens = tokenize(src)
    val parser = Parser(tokens)
    val prog = parser.parseProgram()

    val checker = TypeChecker()
    checker.checkProgram(prog)
    if (checker.errors.isEmpty()) {
        println("Program type checks successfully.")
    } else {
        println("Errors:")
        checker.errors.forEach { println(" - $it") }
    }
}