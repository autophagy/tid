module Tests exposing (..)

import Expect
import Test exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "A" <| \_ -> Expect.equal 1 1 ]
