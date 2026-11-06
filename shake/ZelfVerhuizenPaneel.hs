{-# LANGUAGE OverloadedStrings #-}

-- | De "Te duur?"-knop in de prijskaart van elke landingspagina en het
-- paneel dat hij van rechts laat inschuiven: de aankondiging van de
-- DIY-exporter met een nieuwsbrief-aanmelding. De aanmelding post naar
-- POST /api/offerte met soort=nieuwsbrief en de landingspagina als
-- herkomst (megavid WebshopScanner.OfferteAanvraag): die logt elke
-- aanmelding durabel, mailt hem naar Jappie en zet de shop in de
-- outreach-ledger, zodat hij geen koude mail meer krijgt. Een gratis
-- mailadres (gmail en dergelijke) dat nog niet in de ledger staat, noemt
-- geen shop; dan meldt de notificatiemail dat het met de hand moet.
--
-- Decision: een eigen moduul in plaats van nog een blok in
-- 'WebwinkelTemplates', dat al ver over de 1000 regels zit.
module ZelfVerhuizenPaneel
  ( LandingsPagina(..)
  , herkomstToken
  , teDuurKnop
  , zelfVerhuizenPaneel
  ) where

import Data.Text (Text)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import PageChrome (customAttribute, honeypotVeld, toHtml, toValue)

-- | De pagina's met een prijskaart. De herkomst van een aanmelding laat
-- zien op welk platform winkeliers de migratie te duur vinden, en dus
-- voor welk platform de exporter eerst moet groeien.
data LandingsPagina
  = Homepagina
  | MijnWebwinkelPagina
  | CcvShopPagina
  | LightspeedPagina
  | MagentoPagina
  | ZilverCmsPagina
  deriving (Bounded, Enum, Eq, Show)

-- | Wat het endpoint als herkomst logt; stabiel, want het staat in het
-- aanmeldingenlogboek.
herkomstToken :: LandingsPagina -> Text
herkomstToken pagina = case pagina of
  Homepagina -> "home"
  MijnWebwinkelPagina -> "mijnwebwinkel"
  CcvShopPagina -> "ccvshop"
  LightspeedPagina -> "lightspeed"
  MagentoPagina -> "magento"
  ZilverCmsPagina -> "zilvercms"

-- | Waar de winkelier vandaan verhuist, zoals het in een zin past.
bronInZin :: LandingsPagina -> Text
bronInZin pagina = case pagina of
  Homepagina -> "je huidige webshop"
  MijnWebwinkelPagina -> "MijnWebwinkel"
  CcvShopPagina -> "CCV Shop"
  LightspeedPagina -> "Lightspeed"
  MagentoPagina -> "Magento"
  ZilverCmsPagina -> "ZilverCMS"

-- | Het anker werkt ook zonder JavaScript: dan staat het paneel open in
-- de pagina (zie 'zelfVerhuizenPaneel') en springt de link ernaartoe.
teDuurKnop :: Html
teDuurKnop =
  H.a ! A.href "#zelf-verhuizen" ! A.class_ "cta-button-secondary te-duur-knop" $ "Te duur?"

zelfVerhuizenPaneel :: LandingsPagina -> Html
zelfVerhuizenPaneel pagina = do
  H.dialog ! A.id "zelf-verhuizen" ! A.class_ "zelf-verhuizen-paneel"
           ! customAttribute "aria-labelledby" "zelf-verhuizen-kop" $ do
    H.form ! A.method "dialog" ! A.class_ "paneel-sluiten-form" $
      H.button ! A.class_ "paneel-sluiten" ! customAttribute "aria-label" "Sluiten" $ H.preEscapedToHtml ("&times;" :: Text)
    H.p ! A.class_ "aankondiging-label" $ "Binnenkort"
    H.h2 ! A.id "zelf-verhuizen-kop" $ "Zelf verhuizen?"
    H.p "We maken de DIY-exporter, een zelfbedieningsversie van ons migratieprogramma."
    H.p $ toHtml ("Daarmee haal je je gegevens zelf uit " <> bronInZin pagina <> " en zet je ze zelf in je nieuwe shop. Je betaalt alleen voor wat je verhuist, zonder offerte of abonnement.")
    H.p "Zo doe je de verhuizing zelf en bespaar je flink op de kosten van een volledige migratie."
    H.h3 "Hoor het als eerste"
    H.p "Meld je aan voor de nieuwsbrief. Zodra de DIY-exporter klaar is, krijg je bericht."
    H.form ! A.class_ "nieuwsbrief-formulier" ! A.action "/api/offerte" ! A.method "post" $ do
      H.label ! A.class_ "calc-field" $ do
        H.span ! A.class_ "calc-label" $ "Je e-mailadres"
        H.input ! A.type_ "email" ! A.name "email" ! A.required "required"
                ! A.autocomplete "email" ! A.placeholder "naam@voorbeeld.nl"
      H.input ! A.type_ "hidden" ! A.name "soort" ! A.value "nieuwsbrief"
      H.input ! A.type_ "hidden" ! A.name "herkomst" ! A.value (toValue (herkomstToken pagina))
      -- shop en bericht reizen mee omdat het endpoint ze verwacht; het
      -- bericht noemt de pagina ook, voor een endpoint dat herkomst nog
      -- niet kent.
      H.input ! A.type_ "hidden" ! A.name "shop" ! A.value ""
      H.input ! A.type_ "hidden" ! A.name "bericht"
              ! A.value (toValue ("Aanmelding nieuwsbrief via Te duur? op de " <> herkomstToken pagina <> "-pagina"))
      honeypotVeld
      H.button ! A.type_ "submit" ! A.class_ "cta-button" $ "Aanmelden"
    H.p ! A.class_ "nieuwsbrief-noot" $ "We mailen je alleen over Webwinkelverhuis en de DIY-exporter. Afmelden kan altijd: antwoord op een nieuwsbrief en we halen je van de lijst."
  -- Zonder JavaScript kan het paneel niet modaal openen; dan staat het
  -- gewoon in de pagina, waar de knop naartoe springt.
  H.noscript $ H.style ".zelf-verhuizen-paneel{display:block;position:static;width:auto;height:auto;max-height:none;margin:2rem auto 0;animation:none}.paneel-sluiten-form{display:none}"
  H.script $ H.preEscapedToHtml paneelScript

-- | Opent het paneel modaal en sluit het bij een klik op de achtergrond.
-- Indrukken en loslaten moeten allebei buiten vallen: een sleep vanuit
-- het invoerveld telt anders als achtergrondklik. Alleen het paneel zelf
-- als doel telt, want een toetsenbord-klik op een knop heeft punt 0,0.
paneelScript :: Text
paneelScript =
  "(function(){var p=document.getElementById('zelf-verhuizen');"
    <> "var k=document.querySelector('a.te-duur-knop');"
    <> "if(!p||!k||typeof p.showModal!=='function')return;"
    <> "k.addEventListener('click',function(e){e.preventDefault();p.showModal();"
    <> "if(window.gtag){gtag('event','te_duur_geopend');}});"
    <> "function buiten(e){if(e.target!==p)return false;var r=p.getBoundingClientRect();"
    <> "return e.clientX<r.left||e.clientX>r.right||e.clientY<r.top||e.clientY>r.bottom;}"
    <> "var ingedruktBuiten=false;"
    <> "p.addEventListener('pointerdown',function(e){ingedruktBuiten=buiten(e);});"
    <> "p.addEventListener('click',function(e){if(ingedruktBuiten&&buiten(e)){p.close();}ingedruktBuiten=false;});"
    <> "})();"
