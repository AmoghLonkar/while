load harness

@test "own-1" {
  check 'if true then y := 1 else z := 1' '{y → 1}'
}

@test "own-2" {
  check 'while false do x := 1' '{}'
}

@test "own-3" {
  check 'if ¬ true then x := 1 else z := 3' '{z → 3}'
}

@test "own-4" {
  check 'while false do x := ( 2 * 6 )' '{}'
}

@test "own-5" {
  check 'while n = 0 do n := 12' '{n → 12}'
}
