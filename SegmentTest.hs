
import Warping.Segment.Class

import Test.HUnit
import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Test.HUnit.Base  ((~?=), Test(TestCase, TestList), (~:), (~=?))
import Test.HUnit.Text (runTestTT)
import Test.HUnit.Tools (assertRaises)

perpPoint_tests = test [  "origin"  ~: (Point 0 0)       ~=? (pointPerp $ Point 0 0)
                        , "0"       ~: (Point 0 1)       ~=? (pointPerp $ Point 1 0)
                        , "Pi/4"    ~: (Point (-1) 1)    ~=? (pointPerp $ Point 1 1)
                        , "Pi/2"    ~: (Point (-1) 0)    ~=? (pointPerp $ Point 0 1)
                        , "3Pi/4"   ~: (Point (-1) (-1)) ~=? (pointPerp $ Point (-1) 1)
                        , "Pi"      ~: (Point 0 (-1))    ~=? (pointPerp $ Point (-1) 0)
                        , "5Pi/4"   ~: (Point 1 (-1))    ~=? (pointPerp $ Point (-1) (-1))
                        , "3Pi/2"   ~: (Point 1 0)       ~=? (pointPerp $ Point 0 (-1))
                        , "7Pi/4"   ~: (Point 1 1)       ~=? (pointPerp $ Point 1 (-1))]

pointSubtract_tests = test [  "0 sub 0" ~: (Point 0 0)       ~=? (pointSubtract (Point 0 0) (Point 0 0))
                            , "0 sub +" ~: (Point (-1) (-1)) ~=? (pointSubtract (Point 0 0) (Point 1 1))
                            , "0 sub -" ~: (Point 1 1)       ~=? (pointSubtract (Point 0 0) (Point (-1) (-1)))
                            , "+ sub 0" ~: (Point 2 2)       ~=? (pointSubtract (Point 2 2) (Point 0 0))
                            , "+ sub +" ~: (Point 1 1)       ~=? (pointSubtract (Point 2 2) (Point 1 1))
                            , "+ sub -" ~: (Point 3 3)       ~=? (pointSubtract (Point 2 2) (Point (-1) (-1)))
                            , "- sub 0" ~: (Point (-2) (-2)) ~=? (pointSubtract (Point (-2) (-2)) (Point 0 0))
                            , "- sub +" ~: (Point (-3) (-3)) ~=? (pointSubtract (Point (-2) (-2)) (Point 1 1))
                            , "- sub -" ~: (Point (-1) (-1)) ~=? (pointSubtract (Point (-2) (-2)) (Point (-1) (-1)))]

pointScalarMult_tests = test [  "2" ~: (Point 4 4)          ~=? (pointScalarMult (2 :: Float)    (Point 2 2))
                              , "1" ~: (Point 2 2)          ~=? (pointScalarMult (1 :: Float)    (Point 2 2))
                              , "0.5" ~: (Point 1 1)        ~=? (pointScalarMult (0.5 :: Float)  (Point 2 2))
                              , "0" ~: (Point 0 0)          ~=? (pointScalarMult (0 :: Float)    (Point 2 2))
                              , "-0.5" ~: (Point (-1) (-1)) ~=? (pointScalarMult (-0.5 :: Float) (Point 2 2))
                              , "-1" ~: (Point (-2) (-2))   ~=? (pointScalarMult (-1 :: Float)   (Point 2 2))
                              , "-2" ~: (Point (-4) (-4))   ~=? (pointScalarMult (-2 :: Float)   (Point 2 2))]

pointDot_tests = test [   "Pi/4 dot ||"     ~: 14    ~=? (pointDot (Point 2 2)    (Point 3 4))
                        , "Pi/4 dot +Pi/2"  ~: 0     ~=? (pointDot (Point 2 2)    (Point (-1) 1)) 
                        , "Pi/4 dot -Pi/2"  ~: 0     ~=? (pointDot (Point 2 2)    (Point 1 (-1)))
                        , "Pi/4 dot 0"      ~: 0     ~=? (pointDot (Point 2 2)    (Point 0 0))
                        , "Pi/4 dot -||"    ~: (-14) ~=? (pointDot (Point 2 2)    (Point (-3) (-4)))
                        , "Pi/4 dot +Pi/4"  ~: 2     ~=? (pointDot (Point 2 2)    (Point 0 1))
                        , "Pi/4 dot -Pi/4"  ~: 2     ~=? (pointDot (Point 2 2)    (Point 1 0))
                        , "3Pi/4 dot ||"    ~: 12    ~=? (pointDot (Point (-2) 2) (Point (-2) 4))
                        , "3Pi/4 dot +Pi/2" ~: 0     ~=? (pointDot (Point (-2) 2) (Point (-1) (-1)))
                        , "3Pi/4 dot -Pi/2" ~: 0     ~=? (pointDot (Point (-2) 2) (Point 1 1))
                        , "3Pi/4 dot 0"     ~: 0     ~=? (pointDot (Point (-2) 2) (Point 0 0))
                        , "3Pi/4 dot -||"   ~: (-4)  ~=? (pointDot (Point (-2) 2) (Point 1 (-1)))
                        , "3Pi/4 dot +Pi/4" ~: 2     ~=? (pointDot (Point (-2) 2) (Point (-1) 0))
                        , "3Pi/4 dot -Pi/4" ~: 2     ~=? (pointDot (Point (-2) 2) (Point 0 1))]

pointNorm_tests = test [  "Pi/4"  ~: 5 ~=? (pointNorm $ Point 3 4)
                        , "3Pi/4" ~: 5 ~=? (pointNorm $ Point (-3) 4)
                        , "5Pi/4" ~: 5 ~=? (pointNorm $ Point (-3) (-4))
                        , "7Pi/4" ~: 5 ~=? (pointNorm $ Point 3 (-4))]


-- Segment Tests

-- UV Tests
getU_tests = test [   "getU (0,0,0,2)(1,1)" ~: 0.5  ~=? segGetU (Segment 0 0 0 2) (Point 1 1)
                    , "getU (10, 10, 10, 30) (15,20)" ~: 0.5 ~=? segGetU (Segment 10 10 10 30) (Point 15 20)]
getV_tests = test [   "getV (0,0,0,2)(1,1)" ~: (-1) ~=? segGetV (Segment 0 0 0 2) (Point 1 1)
                    , "getV (10, 10, 10, 30) (15,20)" ~: (-5) ~=? segGetV (Segment 10 10 10 30) (Point 15 20)]


-- distPointToSegment Tests
distance_tests = test [   "VBar Closer to P right" ~: 5 ~=? distPointToSegment (Segment 10 10 10 30) (Point 7 6)
                        , "VBar Closer to P left"  ~: 5 ~=? distPointToSegment (Segment 10 10 10 30) (Point 13 6)
                        , "VBar On line"           ~: 0 ~=? distPointToSegment (Segment 10 10 10 30) (Point 10 20)
                        , "VBar left line"         ~: 5 ~=? distPointToSegment (Segment 10 10 10 30) (Point 5 20)

                        , "VBar right line"        ~: 5 ~=? distPointToSegment (Segment 10 10 10 30) (Point 15 20)

                        , "VBar Closer to Q left"  ~: 5 ~=? distPointToSegment (Segment 10 10 10 30) (Point 7 34)
                        , "VBar Closer to Q right" ~: 5 ~=? distPointToSegment (Segment 10 10 10 30) (Point 13 34)]

tests_all = TestList [    TestLabel "perpPoint" perpPoint_tests
                        , TestLabel "pointSubtract" pointSubtract_tests
                        , TestLabel "pointScalarMult" pointScalarMult_tests
                        , TestLabel "pointDot" pointDot_tests
                        , TestLabel "pointNorm" pointNorm_tests
                        , TestLabel "getU" getU_tests
                        , TestLabel "getV" getV_tests
                        , TestLabel "distance_tests" distance_tests]

main = runTestTT tests_all