module Modeliero.Codegen.Templates.ReexportsModuleSpec where

import Coalmine.Prelude
import Modeliero.Codegen.ExtrasFor.Hspec
import Modeliero.Codegen.Templates.ReexportsModule qualified as Subject
import Test.Hspec

spec :: Spec
spec = do
  describe "1" do
    let params =
          Subject.Params
            { models =
                [ "Z.V.A",
                  "Z.V.B"
                ]
            }

    compilingProducesExpectedContent Subject.compile params
      $ [i|      
            module Z.V
              ( module Z.V.A,
                module Z.V.B,
              )
            where
            
            import Z.V.A
            import Z.V.B
          |]
