module Tests exposing (all)

import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "A Test Suite"
        [ test "A" <| \_ -> Expect.equal 1 1 ]
