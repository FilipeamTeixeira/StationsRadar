<template>
<div>
<v-container fluid text-xs-center ma-0 pa-0>
  <v-row no-gutters> 
        <v-card
        color="rgb(29,172,232)"
          flat
          tile
          width="100%"
          max-height="10px">
           <p></p>
    </v-card>
    </v-row>
  <div>
    <v-container>
      <v-row cols="auto" sm="5" class = "pl-4">
        <v-col cols="auto" sm="4">
    <multiselect
              @input="triggerSetSelectedStations_line"
              v-model="selected"
              :options="selected.length < 12 ? $store.state.stations_line: []"
              label="station"
              :value="selectedStations_line"
              :multiple="true"
              :close-on-select="false"
              :clear-on-select="false"
              :preserve-search="true"
              placeholder="Selecteer maximaal 12 stations."
              track-by="station"
              :preselect-first="false"
              hint="Selecteer maximaal 12 stations."
              >
                <template slot="selection" slot-scope="{ values, search, isOpen }">
                <span class="multiselect__single" v-if="values.length &amp;&amp; !isOpen">
                  {{ values.length }} stations selected</span>
                </template>
              </multiselect>
               </v-col>
               <v-col cols="auto" align="end">
               <v-btn
              dark
              small
              class="ma-2"
              
              color="#76A08A"
              @click="updateChart()">Update graph</v-btn>    
            <v-btn
              dark
              small
              class="ma-2"

              color="#EDCB64"
              @click="forceRerender()">Reset</v-btn>
        </v-col>
      </v-row>
    </v-container>
    <v-tabs
      light
      centered
      slider-color="rgb(29,172,232)"
      color="rgb(29,172,232)"
    >
      <v-tab> Auto
      </v-tab>
      <v-tab-item :key=1 :eager="true">
      <lineGraph v-bind:options="lineAuto" v-if="renderComponent"/></v-tab-item>
      
      <v-tab> Fiets
      </v-tab>
      <v-tab-item :key=2 :eager="true">
      <lineGraph v-bind:options="lineFiets" v-if="renderComponent"/></v-tab-item>
      
      <v-tab> Bus, Tram, Metro
      </v-tab>
      <v-tab-item :key=3 :eager="true">
      <lineGraph v-bind:options="lineBTM" v-if="renderComponent"/></v-tab-item>

      <v-tab> Trein
      </v-tab>
      <v-tab-item :key=4 :eager="true">
      <lineGraph v-bind:options="lineTrein" v-if="renderComponent"/></v-tab-item>
      
      <v-tab> Densiteit
      </v-tab>
      <v-tab-item :key=5 :eager="true">
      <lineGraph v-bind:options="lineDen" v-if="renderComponent"/></v-tab-item>

      <v-tab> Diversiteit
      </v-tab>
      <v-tab-item :key=6 :eager="true">
      <lineGraph v-bind:options="lineDiv" v-if="renderComponent"/></v-tab-item>

      <v-tab> Design
      </v-tab>
      <v-tab-item :key=7 :eager="true">
      <lineGraph v-bind:options="lineDesign" v-if="renderComponent"/></v-tab-item>
    
    </v-tabs>
  </div>
</v-container>


<v-row no-gutters> 
        <v-card
        color="rgb(29,172,232)"
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

import lineGraph from '../components/line'

export default {
 data () {
        return{
            renderComponent: true,
            selected: [],

lineAuto:{
xAxis:{
  categories:['',
            'Gratis parking',
            'Betalende parking',
            'Totale parking',
            'Bezetting parking']
},
    yAxis: [{
        categories: []
    }],
series: [{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
}],
},
    lineFiets:{
              xAxis:{
  categories:['',
            'Gratis parking',
            'Betalende parking',
            'Totale parking',
            'Bezetting parking']
},
    yAxis: [{
        categories: []
    }],
series: [{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
}],
},
            lineBTM:{
              xAxis:{
  categories:['',
            'Frequentie',
            'Route']
},
    yAxis: [{
        categories: []
    }],
series: [{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
}],
},

            lineTrein:{
              xAxis:{
  categories:['',
            'Frequentie dinsdag',
            'Frequentie zaterdag',
            'Frequentie off-peak',
            'Amplitude',
            'Reistijdcentraliteit',
            'Overstapcentraliteit']
},
    yAxis: [{
        categories: []
    }],
series: [{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
}],
},

            lineDen:{
              xAxis:{
  categories:['',
            'Inwoners',
            'Jobs',
            'Regionale voorz.',
            'Basisvoorz.',
            'Metropolitane voorz.']
},
    yAxis: [{
        categories: []
    }],
series: [{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
}],
},

            lineDiv:{
              xAxis:{
  categories:['',
            'Functioneel',
            'Ruimtelijk']
},
    yAxis: [{
        categories: []
    }],
series: [{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
}],
},

            lineDesign:{
              xAxis:{
  categories:['',
            'Interface',
            'Permeabiliteit',
            'Fietswandel infrastr.']
},
    yAxis: [{
        categories: []
    }],
series: [{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[]},{data:[],
}],
},
        }
      },
  components: {
lineGraph
  },
  methods: {
  triggerSetSelectedStations_line(val) {
      this.$store.dispatch('setSelectedStations_line', val)
    },

forceRerender() {

  //location.reload()
     //    Remove my-component from the DOM
        this.renderComponent = false;
        
        this.$nextTick(() => {
     //      Add the component back in
          this.renderComponent = true;
        });
          this.selected = []          
      },

  updateChart(){
 
        this.$store.state.selectedStations_line

        //Calculations for computed data
        // Sort stations by name
        this.$store.state.selectedStations_line.sort(function(a,b){
              return a.station.toLowerCase().localeCompare(b.station.toLowerCase());
        });


        var len = this.$store.state.selectedStations_line.length

        var seq =  Array.from({length: len}, (el, index) => index )
    //    var seq_inv = seq.sort().reverse();
      //  var objlen = Array.from({length: 26}, (el, index) => index )
        var mnArr = []




        seq.forEach((i,index) => {
        //  var temp = [];
          var name = this.$store.state.selectedStations_line[index].station;
          var temp2 = [...[index], ...(this.$store.state.selectedStations_line[index].data)];
          mnArr[name] = temp2
        });


        var line_seq = Array.from({length: 27}, (el, index) => index ).reverse()

        var station_names = [...Object.keys(mnArr),
        ...Array.from({length: 27 - Object.keys(mnArr).length}, (el, index) => null )]


        this.lineAuto.yAxis[0].categories = station_names;
        this.lineFiets.yAxis[0].categories = station_names;
        this.lineBTM.yAxis[0].categories = station_names;
        this.lineTrein.yAxis[0].categories = station_names;
        this.lineDen.yAxis[0].categories = station_names;
        this.lineDiv.yAxis[0].categories = station_names;
        this.lineDesign.yAxis[0].categories = station_names;




        line_seq.forEach((i, index) => {
        
           //var lnArr_auto = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           //mnArr[this.$store.state.selectedStations_line[i].station].slice(0,5)

          //Auto
           var lnArr_auto = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           mnArr[this.$store.state.selectedStations_line[i].station].slice(0,5)
           
            var temp_index = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
            mnArr[this.$store.state.selectedStations_line[i].station][0]

            if(temp_index != null){
              lnArr_auto[0] = temp_index
            }

           this.lineAuto.series[i].data = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           lnArr_auto;



          //Fiets
           var lnArr_fiets = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           mnArr[this.$store.state.selectedStations_line[i].station].slice(4,9)
           
            var temp_index = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
            mnArr[this.$store.state.selectedStations_line[i].station][0]

            if(temp_index != null){
              lnArr_fiets[0] = temp_index
            }

           this.lineFiets.series[i].data = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           lnArr_fiets;


           //BTM
           var lnArr_BTM = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           mnArr[this.$store.state.selectedStations_line[i].station].slice(8,11)
           
            var temp_index = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
            mnArr[this.$store.state.selectedStations_line[i].station][0]

            if(temp_index != null){
              lnArr_BTM[0] = temp_index
            }

           this.lineBTM.series[i].data = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           lnArr_BTM;


           //Trein
           var lnArr_trein = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           mnArr[this.$store.state.selectedStations_line[i].station].slice(10,17)

            var temp_index = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
            mnArr[this.$store.state.selectedStations_line[i].station][0]

            if(temp_index != null){
             lnArr_trein[0] = temp_index
            }

           this.lineTrein.series[i].data = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           lnArr_trein;


           //Densiteit
           var lnArr_den = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           mnArr[this.$store.state.selectedStations_line[i].station].slice(16,22)
           
            var temp_index = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
            mnArr[this.$store.state.selectedStations_line[i].station][0]

            if(temp_index != null){
              lnArr_den[0] = temp_index
            }

           this.lineDen.series[i].data = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           lnArr_den;


           //Diversiteit
           var lnArr_div = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           mnArr[this.$store.state.selectedStations_line[i].station].slice(21,24)
           
            var temp_index = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
            mnArr[this.$store.state.selectedStations_line[i].station][0]

            if(temp_index != null){
              lnArr_div[0] = temp_index
            }

           this.lineDiv.series[i].data = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           lnArr_div;

           //Design
           var lnArr_design = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           mnArr[this.$store.state.selectedStations_line[i].station].slice(23,27)
           
            var temp_index = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
            mnArr[this.$store.state.selectedStations_line[i].station][0]

            if(temp_index != null){
              lnArr_design[0] = temp_index
            }

           this.lineDesign.series[i].data = Object.is(this.$store.state.selectedStations_line[i], undefined) ? null :
           lnArr_design;



        });

           
},
    },

computed:{
  stations_line() {
      return this.$store.getters.getStations_line;
    },
    selectedStations_line() {
      return this.$store.getters.getSelectedStations_line;
    },
  },
}
</script>

<style src="vue-multiselect/dist/vue-multiselect.min.css"></style>

