<script src="https://www.google.com/recaptcha/api.js?onload=vueRecaptchaApiLoaded&render=explicit" async defer>
</script> 

<template>
  <div class="about">
<v-container fluid text-xs-center ma-0 pa-0 style="max-height: 100%; background-color: #F5F5F5">

<v-row no-gutters> 
        <v-card
        color="#2E604A"
          flat
          tile
          width="100%"
          max-height="10px">
           <p></p>
    </v-card>
    </v-row>

      <v-row class = "mt-0 ml-12 pl-12 mr-12 pr-12 mb-0" align="center">

      <v-col cols="auto">
      <v-card-text class="text-justify">
      De StationsRadar tool werd ontwikkeld in het kader van recent afgerond doctoraatsonderzoek 
      (december 2019) aan Universiteit Gent en Vrije Universiteit Brussel (gefinancierd door het Fonds 
      Wetenschappelijk Onderzoek). Dit onderzoek, uitgevoerd door 
      <a href="http://www.geografie.ugent.be/medewerkers/802002154951"
      rel="noopener noreferrer" target="_blank"> Freke Caset </a> en getiteld ‘Planning 
      for Nodes, Places, and People: A Strategic Railway Station Development Tool for Flanders’, kunt u hier 
      <a href = "" rel="noopener noreferrer" target="_blank">"link wordt binnenkort ingevoegd"</a> inkijken. Freke was 
      verantwoordelijk voor het dataverzamelings- en verwerkingsproces en voor de 
      conceptuele ontwikkeling van de radardiagrammen. Verder was de intensieve medewerking van UGent-collega 
      en doctoraatsonderzoeker <a href ="https://github.com/FilipeamTeixeira"
      rel="noopener noreferrer" target="_blank">Filipe Teixeira</a> 
      cruciaal in de totstandkoming van deze tool. Filipe heeft de tool geprogrammeerd en verzorgde 
      het design van de website en de grafieken. Het doctoraatsonderzoek en de ontwikkeling van de tool 
      werden gesuperviseerd door Ben Derudder en Frank Witlox (UGent) en Kobe Boussauw (VUB).
      
      <p></p>

      De redenering die aan de basis ligt van de 
      radardiagrammen boogt op een lange traditie van ‘knoop-plaatsmodellen’: een methodiek die 
      tot op vandaag met name in de Nederlandse context wordt toegepast. Binnen deze knoop-plaats 
      benadering wordt de bereikbaarheid van een stationsomgeving niet enkel bekeken vanuit een louter 
      transporttechnische hoek (de knoopwaarde van een station), maar wordt ook de ruimtelijke context 
      waarbinnen het station functioneert (de plaatswaarde) uitdrukkelijk meegenomen. Naast aspecten van 
      ‘knoop’ en ‘plaats’ werden in dit onderzoek ook kenmerken van de stationsgebruikers opgenomen.
      
      </v-card-text>
          </v-col>
       </v-row>


       <v-row class = "mt-4">
           <v-img :src="require('../assets/background.jpg')"  height="200"></v-img>
       </v-row>
      
            <v-row class = "mt-0 ml-12 pl-12 mr-12 pr-12" align="center">

      <v-col cols="auto">
      <v-card-text class="text-justify">


      Een dergelijk knoop-plaats denkkader is dus met name inzetbaar in een planningscontext waar de domeinen 
      mobiliteit en ruimte op een geïntegreerde wijze worden aangepakt. Omwille van die reden besloten we 
      om de bruikbaarheid van de StationsRadar tool te valideren binnen een dergelijke (beoogde) geïntegreerde 
      Vlaamse planningscontext: de recent opgerichte vervoerregio’s. Treinstations behoren tot de belangrijkste 
      mobipunten én knooppunten binnen deze regionale afbakeningen en vormden zodoende de focus van drie 
      workshops die in de regio’s Gent, Aalst en Leuven werden opgezet (periode januari-februari 2019). 
      Het doel van deze workshops was om te peilen naar de bruikbaarheid van StationsRadar in de context 
      van de planningstaken die in de vervoerregio’s op de agenda staan. Aan tafel zaten verschillende 
      stakeholders vanuit de domeinen mobiliteit en ruimte: NMBS, De Lijn, de Provincies, de gemeentes, 
      intercommunales, en de departementen MOW en Omgeving. Indien u meer wil lezen over het verloop en de 
      bevindingen van deze workshops verwijzen we u door naar hoofdstuk 3 van het doctoraat (zie hogerop). 
      Als algemeen besluit kan gesteld worden dat de empirische data die door StationsRadar wordt samengebracht, en de 
      mogelijkheid om interactief aan de slag te gaan met deze data, waardevol wordt geacht tijdens met name de divergerende 
      (verkennende) fases in het planningsproces.
      
      <p></p>

      
      De looptijd van de FWO-beurs waarbinnen StationsRadar 
      werd ontwikkeld is afgerond. Financiering en middelen om deze tool op regelmatige basis te updaten 
      en verder uit te bouwen ontbreken dus. Wij zullen in ieder geval het gebruik van deze tool evalueren 
      en op basis daarvan bekijken hoe dit project desgewenst kan worden verdergezet. Voor verdere vragen of 
      opmerkingen kunt u ons bereiken via
      <template>
    <v-dialog v-model="dialog" persistent max-width="600px">
      <template v-slot:activator="{ on }">
        <v-btn color="primary" small text dark v-on="on">Deze link</v-btn>
      </template>
      <v-card>
        <v-card-title>
          <span class="headline">Contacteer ons</span>
        </v-card-title>
        <v-card-text>
          <v-container>

      <v-row>
         <v-col cols = "12">
           <v-form v-model="valid" method = "post"
           class="gform pure-form pure-form-stacked"
           data-email="example@email.net"
           action="https://script.google.com/macros/s/AKfycbxtKzAkTcFVvr3HRqrHpDVZS5jstQPsHAfsLmSkqGekhVQeBpqq/exec"
           id = "nativeForm"
           target="_blank">
    <v-container>
      <v-row align="end" justify="end">
        <v-col
          cols="12"
        >
          <v-text-field
            v-model="name"
            :rules="nameRules"
            :counter="15"
            label="Naam*"
            required
            name="name"
          ></v-text-field>
        </v-col>
        <v-col
          cols="12"
        >
          <v-text-field
            v-model="email"
            :rules="emailRules"
            label="E-mail*"
            required
            name="email"
          ></v-text-field>
        </v-col>

      </v-row>
      <v-row justify="end" align="end">
      
      <v-col cols="12">

          <v-textarea
            v-model="message"
            :rules="messageRules"
            counter
            minlength="20"
            maxlength="350"
            label="Bericht*"
            required
            name="message"
          >

          </v-textarea>
        </v-col>

      </v-row>
    </v-container>
  </v-form>
         </v-col>
       </v-row>

          </v-container>
          <small>*Verplicht veld</small>
        </v-card-text>
        <v-card-actions>
          <v-spacer></v-spacer>
        <v-btn color="blue darken-1" text @click="dialog = false">Sluit</v-btn>
    <vue-recaptcha sitekey="6Lf6LMEUAAAAAGojNKKSvnOp__P3CYjNcL69uPEt">
               <v-btn
        :disabled="!valid"
        color="success"
        class="mr-4"
        text
        v-on:click="submit"
      >
        Verstuur
      </v-btn>
</vue-recaptcha>
        </v-card-actions>
      </v-card>
    </v-dialog>
</template>

      <p></p>

Tot slot willen we NMBS graag bedanken voor de samenwerking en voor het 
aanleveren van de gebruikersdata in het kader van dit project.  
      </v-card-text>
          </v-col>
       </v-row>

       <v-row class = "mt-10">
           <v-img :src="require('../assets/station1.jpg')"  height="200"></v-img>
       </v-row>

<v-row class = "mt-0 ml-12 pl-12 mr-12 pr-12 mb-0" align="center">

      <v-col cols="auto">

        <v-card-title>
          Met dank aan
          </v-card-title>
      <v-card-text class="text-justify">

  Deze website maakt gebruik van Ubuntu, R, RStudio Server, 
  Vue.js en Vuetify. De kaarten werden gecreëerd dankzij OpenStreetMap en Leaflet. 
  Alle grafieken werden gecreëerd met Highcharts.
  </v-card-text>
  
  </v-col>
  </v-row>
    <v-row class = "mt-0 ml-12 pl-12 mr-12 pr-12 mb-0" align="center">
    <v-col cols = "3">
    <v-card flat tile class="d-flex"  color="#F5F5F5">
                <v-img
                  :src="require('../assets/Highcharts.png')"
                  height = "100px"
                  contain
                >
                </v-img>
                </v-card>
      </v-col>
      <v-col cols = "3">
          <v-card flat tile class="d-flex" color="#F5F5F5">
                <v-img
                  :src="require('../assets/RStudio.png')"
                  contain
                  height = "60px"
                >
                </v-img>
                </v-card>
      </v-col>
            <v-col cols = "3">
          <v-card flat tile class="d-flex" color="#F5F5F5">
                <v-img
                  :src="require('../assets/leaflet.png')"
                  height = "50px"
                  contain
                >
                </v-img>
                </v-card>
      </v-col>
            <v-col cols = "3">
          <v-card flat tile class="d-flex" color="#F5F5F5">
                <v-img
                  :src="require('../assets/osm.png')"
                  height = "70px"
                  contain
                >
                </v-img>
                </v-card>
      </v-col>
    </v-row>

  <v-row class = "mt-0 ml-12 pl-12 mr-12 pr-12 mb-0" align="center">
    <v-col cols = "3">
    <v-card flat tile class="d-flex" color="#F5F5F5">
                <v-img
                  :src="require('../assets/vue.png')"
                  height = "80px"
                  contain
                >
                </v-img>
                </v-card>
      </v-col>
      <v-col cols = "3">
          <v-card flat tile class="d-flex" color="#F5F5F5">
                <v-img
                  :src="require('../assets/vuetify.svg')"
                  contain
                  height = "80px"
                >
                </v-img>
                </v-card>
      </v-col>
            <v-col cols = "3">
          <v-card flat tile class="d-flex" color="#F5F5F5">
                <v-img
                  :src="require('../assets/ubuntu.png')"
                  height = "80px"
                  contain
                >
                </v-img>
                </v-card>
      </v-col>
    </v-row>




</v-container>

<v-row no-gutters> 
        <v-card
        color="#2E604A"
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

import jsPDF from 'jspdf'
import VueRecaptcha from 'vue-recaptcha';

 export default {
    components: { VueRecaptcha },
    data(){
    return {
    pdfname: '',
    valid: true,
          dialog: false,

      name: '',
      nameRules: [
        v => !!v || 'Name is required',
        v => v.length <= 15 || 'Name must be less than 15 characters',
      ],
      message: '',
      messageRules: [
        v => !!v || 'Message is required',
        v => v.length >= 10 || 'Message must be more than 10 characters',
      ],
      email: '',
      emailRules: [
        v => !!v || 'E-mail is required',
        v => /.+@.+/.test(v) || 'E-mail must be valid',
      ],
    }},
        methods: {
                validate () {
        if (this.$refs.form.validate()) {
          this.snackbar = true
        }
      },
      submit() {
//        nativeForm.preventDefault();
        nativeForm.submit();
        this.dialog = false;
        
        }
    }
  }
</script>

<style scoped>
A {text-decoration: none;}
</style>


