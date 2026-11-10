{-# LANGUAGE OverloadedStrings #-}

-- | De "Te duur?"-knop in de prijskaart van elke landingspagina en het
-- paneel dat hij van rechts laat inschuiven. Kan de DIY-exporter het
-- platform van de pagina al uitlezen, dan wijst het paneel ernaartoe;
-- anders kondigt het hem aan met een nieuwsbrief-aanmelding. Die post naar
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
  , ExporterStand(..)
  , exporterStand
  , exporterAanmeldUrl
  , welkomstCredits
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
  H.a ! A.href "#zelf-verhuizen" ! A.class_ "te-duur-knop" $ "Te duur?"

-- | Of de exporter het platform van een pagina al kan uitlezen. Waar hij
-- dat kan, wijst het paneel naar de exporter; elders blijft het een
-- aankondiging met nieuwsbrief.
data ExporterStand
  = ExporterBeschikbaar
  | ExporterBinnenkort
  deriving (Eq, Show)

exporterStand :: LandingsPagina -> ExporterStand
exporterStand pagina = case pagina of
  Homepagina -> ExporterBinnenkort
  MijnWebwinkelPagina -> ExporterBeschikbaar
  CcvShopPagina -> ExporterBinnenkort
  LightspeedPagina -> ExporterBinnenkort
  MagentoPagina -> ExporterBinnenkort
  ZilverCmsPagina -> ExporterBinnenkort

-- | De aanmelding van de exporter in megavid's klantenportaal. Een nieuwe
-- bezoeker heeft nog geen account, en de pagina linkt zelf door naar de
-- inlog.
--
-- Decision: geen utm-parameters op deze link. Het portaal meet in
-- dezelfde GA4-property (G-GD4S885G6F) als webwinkelverhuis.nl, en utm
-- op een interne link begint daar een nieuwe sessie met deze pagina als
-- bron, waardoor de echte herkomst van de bezoeker verloren gaat. De
-- klik telt als exporter_klik ('WebwinkelTemplates.ctaTrackScript').
exporterAanmeldUrl :: H.AttributeValue
exporterAanmeldUrl = "https://klanten.webwinkelverhuis.nl/exporter/registreren"

-- | Megavid's welkomstsaldo (Application.CreditTarief), zoals de
-- pagina's het beloven; verandert dat, dan dit ook.
welkomstCredits :: Text
welkomstCredits = "100"

zelfVerhuizenPaneel :: LandingsPagina -> Html
zelfVerhuizenPaneel pagina = do
  H.dialog ! A.id "zelf-verhuizen" ! A.class_ "zelf-verhuizen-paneel"
           ! customAttribute "aria-labelledby" "zelf-verhuizen-kop" $ do
    H.form ! A.method "dialog" ! A.class_ "paneel-sluiten-form" $
      H.button ! A.class_ "paneel-sluiten" ! customAttribute "aria-label" "Sluiten" $ H.preEscapedToHtml ("&times;" :: Text)
    case exporterStand pagina of
      ExporterBeschikbaar -> exporterAanbod pagina
      ExporterBinnenkort -> exporterAankondiging pagina
  -- Zonder JavaScript kan het paneel niet modaal openen; dan staat het
  -- gewoon in de pagina, waar de knop naartoe springt.
  H.noscript $ H.style ".zelf-verhuizen-paneel{display:block;position:static;width:auto;height:auto;max-height:none;margin:2rem auto 0;animation:none}.paneel-sluiten-form{display:none}"
  H.script $ H.preEscapedToHtml paneelScript

-- | Het productbestand van de exporter is alleen voor Shopify, dus wie
-- naar een ander platform gaat, wordt naar de volledige migratie
-- gestuurd.
exporterAanbod :: LandingsPagina -> Html
exporterAanbod pagina = do
  H.p ! A.class_ "aankondiging-label" $ "Nieuw"
  H.h2 ! A.id "zelf-verhuizen-kop" $ "Verhuizen voor een lager bedrag?"
  H.p $ toHtml ("Dat kan nu zelf. Je haalt je producten uit " <> bronInZin pagina <> " als bestand dat je zo in Shopify importeert, en je orders met alle orderregels als bestand voor Excel.")
  H.p $ toHtml ("Je begint met " <> welkomstCredits <> " gratis credits. Daarna betaal je alleen voor wat je ophaalt, zonder offerte of abonnement.")
  H.p "Het importeren in Shopify doe je zelf, net als de rest van de verhuizing: je thema, je domeinnaam en de doorverwijzingen die je plek in Google bewaren."
  H.a ! A.href exporterAanmeldUrl ! A.class_ "cta-button paneel-knop" $ "Probeer het gratis"
  H.p ! A.class_ "paneel-noot" $ do
    "Het productbestand is voorlopig alleen voor Shopify. Ga je naar een ander platform, of doen wij het liever voor je? "
    H.a ! A.href "/prijzen.html#rekenhulp" $ "Bereken je prijs voor de volledige migratie"
    "."

-- | De aanmelding post naar POST /api/offerte met soort=nieuwsbrief en
-- de landingspagina als herkomst; zie de moduulkop.
exporterAankondiging :: LandingsPagina -> Html
exporterAankondiging pagina = do
  H.p ! A.class_ "aankondiging-label" $ "Binnenkort"
  H.h2 ! A.id "zelf-verhuizen-kop" $ "Verhuizen voor een lager bedrag?"
  H.p "We werken aan een hulpmiddel waarmee je zelf je webshop kunt verhuizen."
  H.p $ toHtml ("Daarmee haal je de gegevens uit " <> bronInZin pagina <> " en zet je ze over naar je nieuwe webshop. Je betaalt alleen voor wat je verhuist, zonder offerte of abonnement.")
  H.p "Jij voert de verhuizing uit, in plaats van die aan ons uit te besteden. Zo bespaar je op de kosten."
  H.h3 "Hoor het als eerste"
  H.p "Meld je aan voor de nieuwsbrief. Je krijgt bericht zodra het hulpmiddel klaar is voor gebruik."
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
  H.p ! A.class_ "paneel-noot" $ "Je ontvangt alleen nieuws over Webwinkelverhuis en dit nieuwe hulpmiddel. Afmelden kan altijd door op een nieuwsbrief te antwoorden."

-- | Opent het paneel modaal en sluit het bij een klik op de achtergrond.
-- Indrukken en loslaten moeten allebei buiten vallen: een sleep vanuit
-- het invoerveld telt anders als achtergrondklik. Alleen het paneel zelf
-- als doel telt, want een toetsenbord-klik op een knop heeft punt 0,0.
paneelScript :: Text
paneelScript =
  "(function(){var paneel=document.getElementById('zelf-verhuizen');"
    <> "var knop=document.querySelector('a.te-duur-knop');"
    <> "if(!paneel||!knop||typeof paneel.showModal!=='function')return;"
    <> "knop.addEventListener('click',function(gebeurtenis){gebeurtenis.preventDefault();paneel.showModal();"
    <> "if(window.gtag){gtag('event','te_duur_geopend');}});"
    <> "function buiten(gebeurtenis){if(gebeurtenis.target!==paneel)return false;"
    <> "var rand=paneel.getBoundingClientRect();"
    <> "return gebeurtenis.clientX<rand.left||gebeurtenis.clientX>rand.right"
    <> "||gebeurtenis.clientY<rand.top||gebeurtenis.clientY>rand.bottom;}"
    <> "var ingedruktBuiten=false;"
    <> "paneel.addEventListener('pointerdown',function(gebeurtenis){ingedruktBuiten=buiten(gebeurtenis);});"
    <> "paneel.addEventListener('click',function(gebeurtenis){"
    <> "if(ingedruktBuiten&&buiten(gebeurtenis)){paneel.close();}ingedruktBuiten=false;});"
    <> "})();"
