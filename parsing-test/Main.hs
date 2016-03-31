module Main where

import Rebase.Prelude hiding (takeWhile)
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import qualified Main.Decoders as Decoders


main =
  defaultMain $
  testGroup "All tests"
  [
    testCase "Primitive" $
    let
      input =
        "{\"year\" : 2001, \"month\": 1, \"day\": 2}"
      result =
        Decoders.valueToByteStringToEither Decoders.date input
      in assertEqual (show result) (Right (2001, 1, 2)) result
    ,
    testCase "In different order" $
    let
      input =
        "{\"month\": 1, \"day\": 2, \"year\" : 2001}"
      result =
        Decoders.valueToByteStringToEither Decoders.date input
      in assertEqual (show result) (Right (2001, 1, 2)) result
    ,
    testCase "With redundant fields" $
    let
      input =
        "{\"redundant1\": \"4\", \"month\": 1, \"redundant2\": \"3\", \"day\": 2, \"year\" : 2001}"
      result =
        Decoders.valueToByteStringToEither Decoders.date input
      in assertEqual (show result) (Right (2001, 1, 2)) result
    ,
    testCase "With trailing fields" $
    let
      input =
        "{\"month\": 1, \"day\": 2, \"year\" : 2001, \"trailing\": \"3\", \"trailing\": \"4\"}"
      result =
        Decoders.valueToByteStringToEither Decoders.date input
      in assertEqual (show result) (Right (2001, 1, 2)) result
    ,
    testCase "Array" $
    let
      input =
        "[2001, 1, 2]"
      result =
        Decoders.valueToByteStringToEither Decoders.date input
      in assertEqual (show result) (Right (2001, 1, 2)) result
    ,
    testCase "Nested object lookup" $
    let
      input =
        "{\"response\":{\"numFound\":1}}"
      result =
        Decoders.valueToByteStringToEither Decoders.solrSelectResponseNumFound input
      in assertEqual (show result) (Right 1) result
    ,
    testCase "solrSelectResponseNumFound" $
    let
      input =
        "{\"responseHeader\":{\"status\":0,\"QTime\":0,\"params\":{\"json\":\"{\\\"query\\\":\\\"text:Akuttmedisinsk*\\\",\\\"offset\\\":0,\\\"limit\\\":100}\",\"wt\":\"json\"}},\"response\":{\"numFound\":1,\"start\":0,\"docs\":[{\"id\":\"ark:9788205464377\",\"text\":[\"Akuttmedisinsk sykepleie: utenfor sykehus utenfor sykehus Boken gir en samlet fremstilling av akuttmedisinske tilstander og skader, tilpasset sykepleiere.\\n\\nL\195\166restoffet er forskningsbasert\\nog bygd opp med observasjoner og tiltak p\195\165 basalt og avansert niv\195\165. Boken er rikt illustrert og inneholder mange kasuistikker som\\nhjelper leseren \195\165 knytte sammen teori og praksis.\\nBoken dekker pensumlitteratur p\195\165 fagomr\195\165det akuttmedisin i bachelorutdanningen i\\nsykepleie. Innholdet har et tydelig prehospitalt fokus, men er ogs\195\165 egnet for sykepleiere som arbeider i sykehus. Boken kan leses\\nmed stort utbytte av helsepersonell som arbeider i ambulansetjenesten, akuttmottak, p\195\165 legevakter og AMK-sentraler.\\nAkuttmedisinsk\\nsykepleie - utenfor sykehus utkom f\195\184rste gang i 2001. I denne nye utgaven er stoffet oppdatert og omfattende revidert, og flere nye\\nkapitler er kommet til.\\nSend SMS med kodeord AKUTTMEDISINSK til 2030 og f\195\165 tilsendt bok portofritt hjem til mobiladressen Haugen, Jan Erik\"],\"_version_\":1530231185253335040}]}}"
      result =
        Decoders.valueToByteStringToEither Decoders.solrSelectResponseNumFound input
      in assertEqual (show result) (Right 1) result
    ,
    testCase "solrSelectResponseDocumentIDs" $
    let
      input =
        "{\"responseHeader\":{\"status\":0,\"QTime\":0,\"params\":{\"json\":\"{\\\"query\\\":\\\"text:Akuttmedisinsk*\\\",\\\"offset\\\":0,\\\"limit\\\":100}\",\"wt\":\"json\"}},\"response\":{\"numFound\":1,\"start\":0,\"docs\":[{\"id\":\"ark:9788205464377\",\"text\":[\"Akuttmedisinsk sykepleie: utenfor sykehus utenfor sykehus Boken gir en samlet fremstilling av akuttmedisinske tilstander og skader, tilpasset sykepleiere.\\n\\nL\195\166restoffet er forskningsbasert\\nog bygd opp med observasjoner og tiltak p\195\165 basalt og avansert niv\195\165. Boken er rikt illustrert og inneholder mange kasuistikker som\\nhjelper leseren \195\165 knytte sammen teori og praksis.\\nBoken dekker pensumlitteratur p\195\165 fagomr\195\165det akuttmedisin i bachelorutdanningen i\\nsykepleie. Innholdet har et tydelig prehospitalt fokus, men er ogs\195\165 egnet for sykepleiere som arbeider i sykehus. Boken kan leses\\nmed stort utbytte av helsepersonell som arbeider i ambulansetjenesten, akuttmottak, p\195\165 legevakter og AMK-sentraler.\\nAkuttmedisinsk\\nsykepleie - utenfor sykehus utkom f\195\184rste gang i 2001. I denne nye utgaven er stoffet oppdatert og omfattende revidert, og flere nye\\nkapitler er kommet til.\\nSend SMS med kodeord AKUTTMEDISINSK til 2030 og f\195\165 tilsendt bok portofritt hjem til mobiladressen Haugen, Jan Erik\"],\"_version_\":1530231185253335040}]}}"
      result =
        Decoders.valueToByteStringToEither Decoders.solrSelectResponseDocumentIDs input
      in assertEqual (show result) (Right ["ark:9788205464377"]) result
  ]
