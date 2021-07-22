<template>
<div>
<v-container fluid text-xs-center ma-0 pa-0 style="background-color: #FFFFFF;">
<v-row no-gutters>
  <v-card
        color="#7496D2"
          flat
          tile
          width="100%"
          max-height="10px">
           <p></p>
    </v-card>
       </v-row>
    <v-row justify = "center" align = "center" class = "pt-10 mt-10 mb-n4">
                <v-col cols = "8">
      <v-card-text class="text-center text-justify subtitle-1">
Op deze pagina geven we een woordje uitleg over de tool en de verschillende 
functies waarover die beschikt. We starten met een algemene toelichting in onderstaande video. 
Verderop vindt u meer specifieke duiding bij de radardiagrammen en hoe deze correct te interpreteren.
      </v-card-text>
                </v-col>
    </v-row>
    <v-row justify = "center" align = "center">
      <v-card-text class="text-center headline">
        Hoe deze tool te gebruiken?
      </v-card-text>
        </v-row>
          <v-row justify = "center" align = "center" class = "pb-6 mb-6">
    <youtube :video-id="'rYEhQhu5mjQ'"></youtube>
    </v-row>
        <v-row justify = "center" align = "center">
      <v-card-text class="text-center headline mb-n8">
      Hoe de radardiagrammen en de indicatoren te interpreteren?
      </v-card-text>
        </v-row>
        <v-row justify = "center" align = "center">
          <v-col cols = "8">
      <v-card-text class="text-center text-justify subtitle-1">
                In dit pdf bestand vindt u een beknopt rapport waarin de structuur van de 
        diagrammen en de berekeningswijzen van de uiteindelijke scores en van de verschillende 
        indicatoren worden toegelicht.  <a href='../static/Woordje uitleg.pdf'> PDF FILE </a>
        <p></p>
        Onderstaande video bespreekt aan de hand van 
        een voorbeeld welke informatie nu precies uit de diagrammen gehaald kan worden en hoe diagrammen 
        onderling met elkaar vergeleken dienen te worden. We raden aan om eerst even het rapport door te 
        nemen vooraleer deze video te bekijken. 
      </v-card-text>
          </v-col>
    </v-row>
    <v-row justify = "center" align = "center" class = "pb-10">
  <youtube :video-id="'n6yLEy4MR84'"></youtube>
</v-row>
</v-container>

<v-row no-gutters> 
        <v-card
        color="#7496D2"
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
  components:{
  },
  data() {
    return {
    }
  }
}
</script>
<style>
A {text-decoration: none;} 
</style>