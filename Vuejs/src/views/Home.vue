<template>
 <div class = "home">
 
 <div class= "d-flex d-sm-none"
>
    <v-snackbar
      v-model="snackbar"
      :multi-line="true"
      top="true"
    >
      This website is not optimized for mobile devices.
      <v-btn
        color="red"
        text
        @click="snackbar = false"
      >
        Close
      </v-btn>
    </v-snackbar>
 </div>

 <v-img :src="require('../assets/background.jpg')" height = "100vh">
  <v-container fluid text-xs-center style="height: 90vh; max-height: 100%;">
     <v-row>
      <v-col cols = "8" class = "pl-8 pt-0" >
        <v-card flat color = "rgb(255,255,255,0.8" height = 'auto' class = "pa-2 mr-12 pr-10">          
         <v-card-text>
           <h1 class="display-2 mb-3 text-xs-left font-weight-light">
             Stations(omgevingen) in vergelijkend perspectief
          </h1>
          <h1 class="headline mb-3 text-xs-left font-weight-light">
             Een interactieve 360°-blik op de unieke bereikbaarheidsprofielen van alle Vlaamse en Brusselse
             stations
          </h1>
        </v-card-text>
        </v-card>
      </v-col>
      </v-row>
  </v-container>
  </v-img>
<v-container fluid class="mx-0 px-0" text-xs-center style="background-color:rgb(28,54,107); height: 20vh"> <!-- style = "height: 25vh" -->
    <v-row align = "center">
    <v-flex xs2>
    </v-flex>
    <v-flex xs1 fill-height>
        <v-card dark tile flat color="rgb(28,54,107)">
          <v-card-text class="display-1 font-weight-light pa-0 ma-0" >287</v-card-text>
          <v-card-text class="headline font-weight-thin pa-0 ma-0">stations</v-card-text>
    </v-card>
    </v-flex>
    <v-flex xs1>
    </v-flex>
    <v-card>
    <v-sheet
          class="d-flex"
          color="#F24D29"
          height="80"
          width='2'
        ></v-sheet>
        </v-card>
    <v-flex xs1>
    </v-flex>
            <v-card dark tile flat color="rgb(28,54,107)" class="pa-4">
          <v-card-text class="display-1 font-weight-light pa-0 ma-0">36</v-card-text>
          <v-card-text class="headline font-weight-thin pa-0 ma-0">indicatoren</v-card-text>
    </v-card>
    <v-flex xs1>
    </v-flex>
    <v-card>
    <v-sheet
          class="d-flex"
          color="#EDCB64"
          height="80"
          width='2'
        ></v-sheet>
    </v-card>
        <v-flex xs1>
    </v-flex>
            <v-card dark tile flat color="rgb(28,54,107)">
          <v-card-text class="display-1 font-weight-light pa-0 ma-0">6</v-card-text>
          <v-card-text class="headline font-weight-thin pa-0 ma-0">dimensies</v-card-text>

    </v-card>
    <v-flex xs1>
    </v-flex>
    <v-card>
    <v-sheet
          class="d-flex"
          color="#D3DDDC"
          height="80"
          width='2'
        ></v-sheet>
    </v-card>
  </v-row>
  </v-container>

<v-container fluid style="background-color:#F5F5F5">
  <v-row align="center">
<!--  <v-row :align='center'>
  <v-col cols="4"> -->
    <v-col cols = "5">
    <Rose v-bind:options="chart1"/>
    </v-col>

    <v-col class = "pt-4 mt-4 mr-5">
<!-- <v-flex xs6 align-center class="pt-4 mt-4"> -->
    <v-card-text align-center class="subtitle-1 font-weight-light text-justify">

      Knooppuntontwikkeling staat hoog op de agenda in Vlaanderen.
      Overheden worden gestimuleerd om een visie uit te stippelen voor wonen, werken,
      voorzieningen en recreatie rond goed bereikbare locaties.
      Treinstations vormen een wezenlijk onderdeel van deze potentiële knooppuntlocaties.
      Door de bestaande spoorinfrastructuur beter te benutten en deze beter af te stemmen op
      ruimtelijke ontwikkelingen, kunnen kansen gecreëerd worden voor een duurzamer mobiliteitssysteem
      in de brede zin. 

      <p></p>
      StationsRadar werd ontwikkeld in het kader van recent afgerond doctoraatsonderzoek aan
      Universiteit Gent en Vrije Universiteit Brussel (december 2019). De tool integreert verschillende
      datasets die samen een licht werpen op het functioneren van de stations binnen vervoersnetwerken 
      en de ruimtelijke structuur in Vlaanderen en het Brussels Hoofdstedelijk Gewest. 
      Voor elk station in beide regio's kan data worden geraadpleegd en gevisualiseerd, 
      en dit volgens de wensen van de gebruiker. Zo kan het algemene functioneren van een 
      station binnen verschillende schaalniveaus worden onderzocht (zoals een corridor, de 
      vervoerregio, de Provincie, ...) en kunnen specifiekere indicatorscores gevisualiseerd 
      worden voor zelf gekozen groepen stations. Het opgenomen kaartmateriaal ondersteunt de 
      interpretaties van deze cijfers. We hopen dat deze tool een bijdragen kan leveren binnen 
      huidige plannings- en beleidsvraagstukken over regionale knooppuntstrategieën, om zodoende 
      de transitie richting een duurzamer mobiliteitssysteem mee vorm te geven. 

    </v-card-text>
</v-col>
</v-row>
</v-container>

<v-row no-gutters> 
        <v-card
        color="#1C366B"
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

<cookie-law theme="blood-orange">
  <div slot-scope="props" class = "font-weight-light body-2">
    <v-row>
      <v-col cols = "8">
              We use Google Analytics to improve the user experience.
          All data is anonymised and never transmited to third parties,
          as it is exclusively used by the creators of this website for optimization
          purposes. <a href="/terms"> Read more. </a>
      </v-col>
      <v-col class = "pl-12 pr-0 ml-12 mr-0">
        <v-btn
          color= "grey"
          depressed
          @click="disableTracking(); props.accept()">Don't accept</v-btn>
      </v-col>
      <v-col class = "pl-0 ml-0 mr-0 pr-0">
          <v-btn
            color="success"
            depressed
            @click="props.close"
          >
            Accept
          </v-btn>
      </v-col>
    </v-row>
  </div>
</cookie-law>

        <v-card-text class="body-2 font-weight-light text-justify">
      © 2019 UGENT
      </v-card-text>
      </v-col>

      <!-- Cookiestuff
      <v-col class ="justify-start">
<v-dialog
      v-model="dialog"
      width="500"
    >
      <template v-slot:activator="{ on }">
        <v-btn
          class="font-weight-light body-2"
          text
          v-on="on"
        >
          Cookie policy
        </v-btn>
      </template>

      <v-card>
        <v-card-title
          class="headline"
          primary-title
        >
          Privacy Policy
        </v-card-title>

        <v-card-text>
          We use Google Analytics to improve the user experience.
          All data is anonymised and never transmited to third parties,
          as it is exclusively used by the creators of this website for optimization
          purposes.
        </v-card-text>

        <v-divider></v-divider>

        <v-card-actions>
          <v-spacer></v-spacer>
          <v-btn text
          color= "grey"
          @click="disableTracking(); dialog = false">Don't accept</v-btn>
          <v-btn
            color="success"
            text
            @click="dialog = false"
          >
            Accept
          </v-btn>
        </v-card-actions>
      </v-card>
    </v-dialog>
      </v-col>
      -->

<v-col cols="6">
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

import Rose from "@/components/Rose";
import CookieLaw from 'vue-cookie-law'

  export default {
    components:{
      Rose,
      CookieLaw 
    },
    data () {
      return{
        snackbar: true,
        dialog: false,
        items: [
          {
            src: "../assets/background.jpg",
          },
          {
            src: 'https://cdn.vuetifyjs.com/images/carousel/sky.jpg',
          },
        ],
chart1: {
        title: {
          text: null
        },
        tooltip: { enabled: false },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
    }
  },
  mounted(){
    var n = Array.from({length: 17}, (el, index) => index );
    n.forEach((i) => {
    this.chart1.series[i].data = [this.$store.state.stations[201].value[i]];
    }),
      this.$ga.page('/')
      
    //  this.snackBar = function(){
    //    if() {
    //    return true
    //  } else {
    //    return false
    //  }}
  },
  methods:{
    disableTracking () {
      this.$ga.disable()
      // from now on analytics is disabled
    },
    enableTracking () {
      this.$ga.enable()
      // from now on analytics is enabled
    }
  },
}





</script>