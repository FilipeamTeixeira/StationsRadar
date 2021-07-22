<template>
<div>
 <v-container fluid text-xs-center ma-0 pa-0>
   <v-row no-gutters>
        <v-card
        color="#B62A3D"
          flat
          tile
          width="100%"
          max-height="10px">
           <p></p>
    </v-card>
   </v-row>
   <v-row>
     <v-col>
       <v-card-text align-center class="caption font-weight-light text-justify">
    In onderstaande tabel vindt u voor elk van de indicatoren een korte beschrijving met meer duiding bij de gebruikte bronnen en hun actualiteit. Ter verduidelijking geven we nog even volgende zaken mee: 
    <ul>
    <li>De bron 'Verachtert et al. (2016) heeft betrekking op de volgende studie die door VITO werd uitgevoerd: Verachtert, E., Mayers, I., Poelmans, L., Van der Meulen, M., Vanhulsel, M., Engelen, G. (2016) Ontwikkelingskansen op basis van knooppuntwaarde en nabijheid voorzieningen. Eindrapport, studie uitgevoerd in opdracht van Ruimte Vlaanderen;</li>
    <li>'GTFS' data staat voor General Transit Feed Specification Data. Deze data (op basis van CSV-bestanden) is vrij beschikbaar en omvat informatie over de actuele dienstregeling van aanbieders openbaar vervoer. Voor meer info verwijzen over dit soort data verwijzen we naar https://developers.google.com/transit/gtfs;</li>
    <li>'BFF' staat voor Bovenlokaal Functioneel Fietsroutenetwerk, 'OSM' staat voor OpenStreetMap en 'GRB' staat voor 'Grootschalig referentiebestand'.</li>
    </ul>
       </v-card-text>
     </v-col>
   </v-row>
      <v-row>
        <v-col cols="3" sm="3" class = "ml-3">
        <v-text-field 
        v-model="search"
        append-icon="search"
        label="Search"
        single-line
        hide-details
      ></v-text-field>
        </v-col>
      </v-row>
 </v-container>
  <v-data-table
    :headers="headers"
    :items="items"
    :items-per-page="34"
    class="elevation-1"
    :search="search"
    group-by=" "
    disable-sort = "true"
  ></v-data-table>

<v-row no-gutters> 
        <v-card
        color="#B62A3D"
          flat
          tile
          width="100%"
          max-height="20px">
           <p></p>
    </v-card>
    </v-row>

<v-footer>
    <v-container fluid text-xs-right>
<v-row justify="end" align="center"
      no-gutters>
      <v-col>
        <v-card-text class="body-2 font-weight-light text-justify">
      © 2019 UGENT
      </v-card-text>
      </v-col>
<v-col cols="1">
<v-img :src="require('../assets/fwo.png')" contain height="40"></v-img>
</v-col>
<v-col cols="1">
<v-img :src="require('../assets/ugent.png')" contain height="40"></v-img>
</v-col>
<v-col cols="1">
 <v-img :src="require('../assets/vub.png')" contain height="40"></v-img>
 </v-col>
</v-row>
</v-container>
</v-footer>

    </div>
</template>

<script>


export default {
  data() {
    return {
              search: '',

      headers:[
        {text: " ", value: " " },
        {text:"INDICATOREN", value:"1"},
        {text:"AFKORTING", value:"2"},
        {text:"BRON", value:"3"},
        {text:"ACTUALITEIT BRONDATA", value:"4"}
        ],
      items:[
{1:"Aantal gratis parkeerplaatsen",2:"Fiets (gratis parking)",3:"NMBS",4:"2018", ' ': "FIETS"},
{1:"Aantal betalende parkeerplaatsen",2:"Fiets (betalende parking)",3:"NMBS",4:"2018", ' ': "FIETS"},
{1:"Bezetting parkeerplaatsen ",2:"Fiets (bezetting parking)",3:"NMBS",4:"2018", ' ': "FIETS"},
{1:"Aantal gratis parkeerplaatsen",2:"Auto (gratis parking)",3:"NMBS",4:"2018", ' ': "AUTO"},
{1:"Aantal betalende parkeerplaatsen",2:"Auto (betalende parking)",3:"NMBS",4:"2018", ' ': "AUTO"},
{1:"Bezetting parkeerplaatsen",2:"Auto (bezetting parking) ",3:"NMBS",4:"2018", ' ': "AUTO"},
{1:"Aantal unieke bus-, tram- en metroroutes aanwezig vlakbij het station (op een afstand van 300 meter)",2:"BTM (route)",3:"O.b.v. GTFS data (De Lijn, MIVB/STIB en TEC)",4:"2018", ' ': "BTM"},
{1:"Frequentie vertrekkende bussen, trams en metro's vlakbij het station (op een afstand van 300 meter)",2:"BTM (frequentie)",3:"O.b.v. GTFS data (De Lijn, MIVB/STIB en TEC)",4:"2018", ' ': "BTM"},
{1:"Frequentie vertrekkende treinen op een dinsdag",2:"Trein (frequentie dinsd.)",3:"NMBS",4:"2018", ' ': "TREIN"},
{1:"Frequentie vertrekkende treinen op een zaterdag",2:"Trein (frequentie zat.) ",3:"NMBS",4:"2018", ' ': "TREIN"},
{1:"Frequentie vertrekkende treinen off-peak",2:"Trein (frequentie off-peak)",3:"NMBS",4:"2018", ' ': "TREIN"},
{1:"Amplitude treinaanbod op een dinsdag (dagdeel waarbinnen treindiensten beschikbaar zijn)",2:"Trein (amplitude)",3:"NMBS",4:"2018", ' ': "TREIN"},
{1:"Reistijdcentraliteit ",2:"Reistijdcentraliteit",3:"O.b.v. GTFS data (NMBS)",4:"2018", ' ': "TREIN"},
{1:"Overstapcentraliteit",2:"Overstapcentraliteit",3:"O.b.v. GTFS data (NMBS)",4:"2018", ' ': "TREIN"},
{1:"De dichtheid aan basisvoorzieningen in de stationsbuurt",2:"Dichtheid (basisvoorz.)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015", ' ': "DICHTHEID"},
{1:"De dichtheid aan regionale voorzieningen in de stationsbuurt",2:"Dichtheid (regionale voorz.)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015", ' ': "DICHTHEID"},
{1:"De dichtheid aan metropolitane voorzieningen in de stationsbuurt",2:"Dichtheid (metropolitane voorz.)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015", ' ': "DICHTHEID"},
{1:"De dichtheid aan jobs in de stationsbuurt",2:"Dichtheid (jobs)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015", ' ': "DICHTHEID"},
{1:"De dichtheid aan inwoners in de stationsbuurt",2:"Dichtheid (inwoners)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2013", ' ': "DICHTHEID"},
{1:"Functionele diversiteit van de stationsbuurt (Shannon index)",2:"Diversiteit (funct.)",3:"O.b.v. Landgebruiksbestand VITO",4:"2013", ' ': "DIVERSITEIT"},
{1:"Ruimtelijke diversiteit van de stationsbuurt (IJI index)",2:"Diversiteit (ruimt.)",3:"O.b.v. Landgebruiksbestand VITO",4:"2013", ' ': "DIVERSITEIT"},
{1:"Interface van de stationsbuurt (indicatie voor de grootte van het scheidingsvlak tussen publieke en private ruimte)",2:"Design (interface)",3:"O.b.v. Wegenregister (Geopunt)",4:"2018", ' ': "DESIGN"},
{1:"Het aantal wegkruispunten in de stationsbuurt",2:"Design (permeabiliteit)",3:"O.b.v. Wegenregister (Geopunt)",4:"2018", ' ': "DESIGN"},
{1:"De totale lengte aan wandel- en fietsinfrastructuur in de stationsbuurt",2:"Design (fietswandel infrastr.)",3:"O.b.v. BFF, OSM, Wegenregister, recreatief fietsnetwerk, GRB",4:"2018", ' ': "DESIGN"},
{1:"% scholieren die dit station als bestemmingsstation gebruiken ",2:"Motivatie (scholieren)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018", ' ': "MOTIVATIE"},
{1:"% studenten die dit station als bestemmingsstation gebruiken ",2:"Motivatie (studenten)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018", ' ': "MOTIVATIE"},
{1:"% werkenden die dit station als bestemmingsstation gebruiken ",2:"Motivatie (werkenden)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018", ' ': "MOTIVATIE"},
{1:"% losse ticketverkoop met dit station als bestemmingsstation ",2:"Motivatie (ander)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018", ' ': "MOTIVATIE"},
{1:"% tickets en abonnementen met dit station als oorsprongstation ",2:"Intensiteit gebruik (oorsprong)",3:"NMBS (o.b.v. abonnementsdata en ticketverkoop)",4:"2018", ' ': "INTENSITEIT GEBRUIK"},
{1:"Aantal opstappers op een werkdag volgens de oktobertellingen ",2:"Intensiteit gebruik (aantal)",3:"NMBS (oktobertellingen)",4:"2018", ' ': "INTENSITEIT GEBRUIK"},
{1:"% tickets en abonnementen met dit station als vertrekstation ",2:"Intensiteit gebruik (bestemming)",3:"NMBS (o.b.v. abonnementsdata en ticketverkoop)",4:"2018", ' ': "INTENSITEIT GEBRUIK"},
{1:"% opstappers in het station dat op minder dan 900 m vogelvluchtafstand van het station woont",2:"Invloedsgebied (wandel)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018", ' ': "INVLOEDSGEBIED"},
{1:"% opstappers in het station dat tussen 900 m en 3 km vogelvluchtafstand van het station woont",2:"Invloedsgebied (fiets)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018", ' ': "INVLOEDSGEBIED"},
{1:"% opstappers van het station dat verder dan 3 km vogelvluchtafstand van het station woont",2:"Invloedsgebied (verder)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018", ' ': "INVLOEDSGEBIED"}
      ]
    }
  },
}
			 				 				 														
</script>
<style>

</style>




{1:"FIETS - Aantal gratis parkeerplaatsen",2:"Fiets (gratis parking)",3:"NMBS",4:"2018", ' ': "FIETS"},
{1:"FIETS - Aantal betalende parkeerplaatsen",2:"Fiets (betalende parking)",3:"NMBS",4:"2018", ' ': "FIETS"},
{1:"FIETS - Bezetting parkeerplaatsen ",2:"Fiets (bezetting parking)",3:"NMBS",4:"2018", ' ': "FIETS"},
{1:"AUTO - Aantal gratis parkeerplaatsen",2:"Auto (gratis parking)",3:"NMBS",4:"2018", ' ': "AUTO"},
{1:"AUTO - Aantal betalende parkeerplaatsen",2:"Auto (betalende parking)",3:"NMBS",4:"2018", ' ': "AUTO"},
{1:"AUTO - Bezetting parkeerplaatsen",2:"Auto (bezetting parking) ",3:"NMBS",4:"2018", ' ': "AUTO"},
{1:"BTM - Aantal unieke bus-, tram- en metroroutes aanwezig vlakbij het station (op een afstand van 300 meter)",2:"BTM (route)",3:"O.b.v. GTFS data (De Lijn, MIVB/STIB en TEC)",4:"2018", ' ': "BTM"},
{1:"BTM - Frequentie vertrekkende bussen, trams en metro's vlakbij het station (op een afstand van 300 meter)",2:"BTM (frequentie)",3:"O.b.v. GTFS data (De Lijn, MIVB/STIB en TEC)",4:"2018", ' ': "BTM"},
{1:"TREIN - Frequentie vertrekkende treinen op een dinsdag",2:"Trein (frequentie dinsd.)",3:"NMBS",4:"2018", ' ': "TREIN"},
{1:"TREIN - Frequentie vertrekkende treinen op een zaterdag",2:"Trein (frequentie zat.) ",3:"NMBS",4:"2018"},
{1:"TREIN - Frequentie vertrekkende treinen off-peak",2:"Trein (frequentie off-peak)",3:"NMBS",4:"2018"},
{1:"TREIN - Amplitude treinaanbod op een dinsdag (dagdeel waarbinnen treindiensten beschikbaar zijn)",2:"Trein (amplitude)",3:"NMBS",4:"2018"},
{1:"TREIN - Reistijdcentraliteit ",2:"Reistijdcentraliteit",3:"O.b.v. GTFS data (NMBS)",4:"2018"},
{1:"TREIN - Overstapcentraliteit",2:"Overstapcentraliteit",3:"O.b.v. GTFS data (NMBS)",4:"2018"},
{1:"DICHTHEID - De dichtheid aan basisvoorzieningen in de stationsbuurt",2:"Dichtheid (basisvoorz.)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015"},
{1:"DICHTHEID - De dichtheid aan regionale voorzieningen in de stationsbuurt",2:"Dichtheid (regionale voorz.)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015"},
{1:"DICHTHEID - De dichtheid aan metropolitane voorzieningen in de stationsbuurt",2:"Dichtheid (metropolitane voorz.)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015"},
{1:"DICHTHEID - De dichtheid aan jobs in de stationsbuurt",2:"Dichtheid (jobs)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2015"},
{1:"DICHTHEID - De dichtheid aan inwoners in de stationsbuurt",2:"Dichtheid (inwoners)",3:"O.b.v. kaarten aangemaakt i.k.v. studie Verachtert et al. (2016)",4:"2013"},
{1:"DIVERSITEIT - Functionele diversiteit van de stationsbuurt (Shannon index)",2:"Diversiteit (funct.)",3:"O.b.v. Landgebruiksbestand VITO",4:"2013"},
{1:"DIVERSITEIT - Ruimtelijke diversiteit van de stationsbuurt (IJI index)",2:"Diversiteit (ruimt.)",3:"O.b.v. Landgebruiksbestand VITO",4:"2013"},
{1:"DESIGN - Interface van de stationsbuurt (indicatie voor de grootte van het scheidingsvlak tussen publieke en private ruimte)",2:"Design (interface)",3:"O.b.v. Wegenregister (Geopunt)",4:"2018"},
{1:"DESIGN - Het aantal wegkruispunten in de stationsbuurt",2:"Design (permeabiliteit)",3:"O.b.v. Wegenregister (Geopunt)",4:"2018"},
{1:"DESIGN - De totale lengte aan wandel- en fietsinfrastructuur in de stationsbuurt",2:"Design (fietswandel infrastr.)",3:"O.b.v. BFF, OSM, Wegenregister, recreatief fietsnetwerk, GRB",4:"2018"},
{1:"MOTIVATIE - % scholieren die dit station als bestemmingsstation gebruiken ",2:"Motivatie (scholieren)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018"},
{1:"MOTIVATE - % studenten die dit station als bestemmingsstation gebruiken ",2:"Motivatie (studenten)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018"},
{1:"MOTIVATIE - % werkenden die dit station als bestemmingsstation gebruiken ",2:"Motivatie (werkenden)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018"},
{1:"MOTIVATIE - % losse ticketverkoop met dit station als bestemmingsstation ",2:"Motivatie (ander)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018"},
{1:"INTENSITEIT GEBRUIK - % tickets en abonnementen met dit station als oorsprongstation ",2:"Intensiteit gebruik (oorsprong)",3:"NMBS (o.b.v. abonnementsdata en ticketverkoop)",4:"2018"},
{1:"INTENSITEIT GEBRUIK - Aantal opstappers op een werkdag volgens de oktobertellingen ",2:"Intensiteit gebruik (aantal)",3:"NMBS (oktobertellingen)",4:"2018"},
{1:"INTENSITEIT GEBRUIK -  % tickets en abonnementen met dit station als vertrekstation ",2:"Intensiteit gebruik (bestemming)",3:"NMBS (o.b.v. abonnementsdata en ticketverkoop)",4:"2018"},
{1:"INVLOEDSGEBIED - % opstappers in het station dat op minder dan 900 m vogelvluchtafstand van het station woont",2:"Invloedsgebied (wandel)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018"},
{1:"INVLOEDSGEBIED - % opstappers in het station dat tussen 900 m en 3 km vogelvluchtafstand van het station woont",2:"Invloedsgebied (fiets)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018"},
{1:"INVLOEDSGEBIED - % opstappers van het station dat verder dan 3 km vogelvluchtafstand van het station woont",2:"Invloedsgebied (verder)",3:"NMBS (o.b.v. abonnementsdata)",4:"2018"}