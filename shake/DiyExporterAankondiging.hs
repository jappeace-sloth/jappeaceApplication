{-# LANGUAGE OverloadedStrings #-}

-- | De aankondiging van de DIY-exporter op de MijnWebwinkel-pagina, als
-- kaart naast de FAQ, met de nieuwsbrief-aanmelding erin. De aanmelding
-- post naar POST /api/offerte met soort=nieuwsbrief (megavid
-- WebshopScanner.OfferteAanvraag): die logt elke aanmelding durabel,
-- mailt hem naar Jappie en zet de shop in de outreach-ledger, zodat hij
-- geen koude mail meer krijgt. Een gratis mailadres (gmail en dergelijke)
-- dat nog niet in de ledger staat, noemt geen shop; dan meldt de
-- notificatiemail dat het met de hand moet.
--
-- Decision: een eigen moduul in plaats van nog een blok in
-- 'WebwinkelTemplates', dat al ver over de 1000 regels zit.
module DiyExporterAankondiging
  ( diyExporterAankondigingKaart
  ) where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import PageChrome (honeypotVeld)

diyExporterAankondigingKaart :: Html
diyExporterAankondigingKaart =
  H.aside ! A.class_ "aankondiging-kaart" ! A.id "zelf-verhuizen" $ do
    H.p ! A.class_ "aankondiging-label" $ "Binnenkort"
    H.h2 "Zelf verhuizen?"
    H.p "We maken de DIY-exporter, een zelfbedieningsversie van ons migratieprogramma. Daarmee haal je je gegevens zelf uit MijnWebwinkel en zet je ze zelf in je nieuwe shop. Je betaalt alleen voor wat je verhuist, zonder offerte of abonnement."
    H.p "Zo doe je de verhuizing zelf en bespaar je flink op de kosten van een volledige migratie."
    H.h3 "Hoor het als eerste"
    H.p "Meld je aan voor de nieuwsbrief. Zodra de DIY-exporter klaar is, krijg je bericht."
    H.form ! A.class_ "nieuwsbrief-formulier" ! A.action "/api/offerte" ! A.method "post" $ do
      H.label ! A.class_ "calc-field" $ do
        H.span ! A.class_ "calc-label" $ "Je e-mailadres"
        H.input ! A.type_ "email" ! A.name "email" ! A.required "required"
                ! A.autocomplete "email" ! A.placeholder "naam@voorbeeld.nl"
      H.input ! A.type_ "hidden" ! A.name "soort" ! A.value "nieuwsbrief"
      -- shop en bericht reizen mee omdat het endpoint ze verwacht; het
      -- bericht vertelt Jappie op welke pagina de aanmelding binnenkwam.
      H.input ! A.type_ "hidden" ! A.name "shop" ! A.value ""
      H.input ! A.type_ "hidden" ! A.name "bericht"
              ! A.value "Aanmelding nieuwsbrief via de DIY-exporter-aankondiging op migrate-mijnwebwinkel.html"
      honeypotVeld
      H.button ! A.type_ "submit" ! A.class_ "cta-button" $ "Aanmelden"
    H.p ! A.class_ "nieuwsbrief-noot" $ "We mailen je alleen over Webwinkelverhuis en de DIY-exporter. Afmelden kan altijd: antwoord op een nieuwsbrief en we halen je van de lijst."
