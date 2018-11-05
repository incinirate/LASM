(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    get_local $lhs
    get_local $rhs
    i32.add)
  (func $cnst (result i32)
    i32.const -123456)
  (func $increment (param $val i32) (result i32)
    get_local $val
    i32.const 1
    i32.add)
  (func $loop (result i32)
    (local $x i32)

    (block
      (loop
        (set_local $x (call $increment (get_local $x)))
        (br_if 1 (i32.eq (get_local $x) (i32.const 50)))
        (br 0)
      )
    )

    i32.const 5
    i32.const 8
    i32.eq
    if
      i32.const 7
    else
      i32.const 4
    end

    i32.add
  )
  (export "add" (func $add))
  (export "cnst" (func $cnst))
  (export "loop" (func $loop)))
