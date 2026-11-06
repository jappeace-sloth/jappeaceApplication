{-# LANGUAGE OverloadedStrings #-}

-- | De aankondiging van de DIY-exporter op de MijnWebwinkel-pagina, met
-- de nieuwsbrief-aanmelding eronder. De aanmelding post naar
-- POST /api/offerte met soort=nieuwsbrief (megavid
-- WebshopScanner.OfferteAanvraag): die logt elke aanmelding durabel,
-- mailt hem naar Jappie en haalt de shop uit de koude outreach.
--
-- Decision: een eigen moduul in plaats van nog een blok in
-- 'WebwinkelTemplates', dat al ver over de 1000 regels zit.
module DiyExporterAankondiging
  ( diyExporterAankondigingSectie
  ) where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import PageChrome (honeypotVeld)

diyExporterAankondigingSectie :: Html
diyExporterAankondigingSectie =
  H.section ! A.class_ "aankondiging" ! A.id "zelf-verhuizen" $
    H.div ! A.class_ "aankondiging-kaart" $ do
      H.p ! A.class_ "aankondiging-label" $ "Binnenkort"
      H.h2 "Liever zelf verhuizen? Dat kan straks met de DIY-exporter"
      H.p "We maken een zelfbedieningsversie van ons migratieprogramma. Daarmee haal je je gegevens zelf uit MijnWebwinkel, in bestanden die Shopify direct inleest, en zet je ze zelf in je nieuwe shop. Je begint gratis en betaalt daarna alleen voor wat je exporteert, zonder offerte of abonnement."
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
