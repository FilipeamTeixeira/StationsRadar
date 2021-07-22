<template>
<div>
<v-container fluid text-xs-center ma-0 pa-0>
  <v-row no-gutters>
        <v-card
        color="#F24D29"
          flat
          tile
          width="100%"
          max-height="10px">
           <p></p>
    </v-card>
<!--<v-flex> width="100%" height="900px" -->
  </v-row>
  <v-row no-gutters>
    <v-col>
      <div> 
    <object type="text/html" data="https://railacc.ugent.be/gtfs" style="overflow:auto; width:100%; height:89vh;">
    </object>
      </div>
      </v-col>
<!--</v-flex> -->
    
  </v-row>
</v-container>
<v-row no-gutters> 
        <v-card
        color="#F24D29"
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
      headers:[
        {text:"", value:"name"},
        {text:"INDICATOREN", value:"indicatoren"},
        {text:"AFKORTING", value:"afkorting"},
        {text:"BROON", value:"broon"},
        {text:"JAAR", value:"jaar"}
        ],
      items:[
        {name: "FIETS"
        }
      ]
    }
  }
}
</script>
<style>
#map{
  height:100%;
}
</style>