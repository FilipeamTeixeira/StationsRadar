<template>
<v-container fluid text-xs-center ma-0 pa-0 :style="[{height:'90vh'}, {'max-height':'100%'}, {'background-color':'#F5F5F5'}]">
<v-row no-gutters> 
        <v-card
        color="#76A08A"
          flat
          tile
          width="100%"
          max-height="10px">
           <p></p>
    </v-card>
    </v-row>
      <v-row>
       <v-col cols="auto" sm="5">
       <v-card class = "pa-4" flat color="#F5F5F5">
           <h1 class="headline mb-3 text-xs-left font-weight-light">
             Selecteer hier de stations die je wil vergelijken. <br>
          </h1>
        </v-card>
        </v-col>
       </v-row>

      <v-row cols="auto" sm="5" class = "pl-4">
        <v-col cols="auto" sm="4">
          <div>
            <multiselect
              @input="triggerSetSelectedStations"
              v-model="selected"
              :options="selected.length < 30 ? $store.state.stations: []"
              label="station"
              :value="selectedStations"
              :multiple="true"
              :close-on-select="false"
              :clear-on-select="false"
              :preserve-search="true"
              placeholder="Selecteer maximaal 30 stations."
              track-by="station"
              :preselect-first="false"
              hint="Selecteer maximaal 30 stations."
              >
                <template slot="selection" slot-scope="{ values, search, isOpen }">
                <span class="multiselect__single" v-if="values.length &amp;&amp; !isOpen">
                  {{ values.length }} stations selected</span>
                </template>
              </multiselect>
            </div>
          </v-col>
          <v-col cols="auto" align="end">
            <v-btn
              dark
              small
              class="ma-2"
              :loading="loading"
              :disabled="loading"
              color="#76A08A"
              @click="updateChart()">Update diagrammen</v-btn>    
            <v-btn
              dark
              small
              class="ma-2"
              :loading="loading"
              :disabled="loading"
              color="#EDCB64"
              @click="forceRerender()">Reset</v-btn>

          </v-col>
          </v-row>

      <v-row row-wrap no-gutters class="grey lighten-4">
      <v-col cols="2">
            <Rose v-bind:options="chart1" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart2" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart3" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart4" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart5" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart6" v-if="renderComponent"/>
      </v-col>
    </v-row>

     <v-row row-wrap no-gutters class="grey lighten-4">
      <v-col cols="2">
            <Rose v-bind:options="chart7" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart8" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart9" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart10" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart11" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart12" v-if="renderComponent"/>
      </v-col>
    </v-row>

<!--code for Freke -->

     <v-row row-wrap no-gutters class="grey lighten-4">
      <v-col cols="2">
            <Rose v-bind:options="chart13" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart14" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart15" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart16" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart17" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart18" v-if="renderComponent"/>
      </v-col>
    </v-row>


     <v-row row-wrap no-gutters class="grey lighten-4">
      <v-col cols="2">
            <Rose v-bind:options="chart19" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart20" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart21" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart22" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart23" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart24" v-if="renderComponent"/>
      </v-col>
    </v-row>

     <v-row row-wrap no-gutters class="grey lighten-4">
      <v-col cols="2">
            <Rose v-bind:options="chart25" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart26" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart27" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart28" v-if="renderComponent"/>
      </v-col>
      <v-col cols="2">
            <Rose v-bind:options="chart29" v-if="renderComponent"/>
      </v-col>
            <v-col cols="2">
            <Rose v-bind:options="chart30" v-if="renderComponent"/>
      </v-col>
    </v-row>

<v-row no-gutters> 
        <v-card
        color="#76A08A"
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

    </v-container>

</template>

<script>

import Rose from "@/components/Rose";
import html2canvas from 'html2canvas';
import jsPDF from 'jspdf';


export default {
      components:{
        Rose,
},
    data () {
      return {
    renderComponent: true,
    selected: [],
    headers:[
      {text: "station" , value: "station"},
      {text: "value", value: "value[0]"}],
chart1: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart2: {
        title: {
          text: []
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart3: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart4: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart5: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },

chart6: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },

chart7: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart8: {
        title: {
          text: []
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart9: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart10: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart11: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },

chart12: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
        },

//code for Freke

chart13: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart14: {
        title: {
          text: []
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart15: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart16: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart17: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },

chart18: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },

chart19: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart20: {
        title: {
          text: []
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart21: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart22: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart23: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },

chart24: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
        },

chart25: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart26: {
        title: {
          text: []
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart27: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart28: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },
chart29: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
      },

chart30: {
        title: {
          text: null
        },
        series: [
          {data: []}, {data: []}, {data: []},  {data: []}, {data: []}, {data: []},
          {data: []}, {data: []}, {data: []},{data: []}, {data: []},
          {data: []},{data: []},{data: []},{data: []}, {data: []}, {data: []}]
        },


      }
    },

methods: {
  triggerSetSelectedStations(val) {
      this.$store.dispatch('setSelectedStations', val)
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
 
      

        //Calculations for computed data
        // Sort stations by name
        this.$store.state.selectedStations.sort(function(a,b){
              return a.station.toLowerCase().localeCompare(b.station.toLowerCase());
        });

        var len = this.$store.state.selectedStations.length
        var seq =  Array.from({length: len}, (el, index) => index )
        var objlen = Array.from({length: 17}, (el, index) => index )
        var mnArr = []



        //Create dataframe for roses
        seq.forEach(i => {
              mnArr[this.$store.state.selectedStations[i].station] = []
           objlen.forEach(k  => {
              mnArr[this.$store.state.selectedStations[i].station][k] = []
           })
        })



            objlen.forEach(k => {
              var arr = []
              seq.forEach(i => {
                arr.push(this.$store.state.selectedStations[i].value[k])
              });
              var maxi = Math.max(...arr)
                  var newArr = []
//                  var ind = null
                    seq.forEach(i => {
                      var newValue = (this.$store.state.selectedStations[i].value[k] == null) ? null : ((this.$store.state.selectedStations[i].value[k] * 10) / maxi)
                      // ((this.$store.state.selectedStations[i].value[k] * 10) / maxi)
                      newArr.push(newValue)
                  mnArr[this.$store.state.selectedStations[i].station][k] = (newValue == null) ? null : Number(newValue.toFixed(2))
                  });
            });

 
    //var m = Array.from({length: this.$store.state.selectedStations.length - 1}, (el, index) => index )
   // var n = [1,2,3,5,7,8,9,11,12,13,14,16,17,18,20,21,22];
    var n = Array.from({length: 17}, (el, index) => index );

        n.forEach((i, index) => {
           this.chart1.series[i].data = Object.is(this.$store.state.selectedStations[0], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[0].station][index]];

           this.chart1.title.text = Object.is(this.$store.state.selectedStations[0], undefined) ? null :
           this.$store.state.selectedStations[0].station;


           this.chart2.series[i].data = Object.is(this.$store.state.selectedStations[1], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[1].station][index]];

           this.chart2.title.text = Object.is(this.$store.state.selectedStations[1], undefined) ? null :
           this.$store.state.selectedStations[1].station;


           this.chart3.series[i].data = Object.is(this.$store.state.selectedStations[2], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[2].station][index]];

           this.chart3.title.text = Object.is(this.$store.state.selectedStations[2], undefined) ? null :
           this.$store.state.selectedStations[2].station;


           this.chart4.series[i].data = Object.is(this.$store.state.selectedStations[3], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[3].station][index]];


           this.chart4.title.text = Object.is(this.$store.state.selectedStations[3], undefined) ? null :
           this.$store.state.selectedStations[3].station;


           this.chart5.series[i].data = Object.is(this.$store.state.selectedStations[4], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[4].station][index]];

           this.chart5.title.text = Object.is(this.$store.state.selectedStations[4], undefined) ? null :
           this.$store.state.selectedStations[4].station;


           this.chart6.series[i].data = Object.is(this.$store.state.selectedStations[5], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[5].station][index]];


          this.chart6.title.text = Object.is(this.$store.state.selectedStations[5], undefined) ? null :
           this.$store.state.selectedStations[5].station;


           this.chart7.series[i].data = Object.is(this.$store.state.selectedStations[6], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[6].station][index]];

           this.chart7.title.text = Object.is(this.$store.state.selectedStations[6], undefined) ? null :
           this.$store.state.selectedStations[6].station;


           this.chart8.series[i].data = Object.is(this.$store.state.selectedStations[7], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[7].station][index]];

           this.chart8.title.text = Object.is(this.$store.state.selectedStations[7], undefined) ? null :
           this.$store.state.selectedStations[7].station;


           this.chart9.series[i].data = Object.is(this.$store.state.selectedStations[8], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[8].station][index]];

           this.chart9.title.text = Object.is(this.$store.state.selectedStations[8], undefined) ? null :
           this.$store.state.selectedStations[8].station;


           this.chart10.series[i].data = Object.is(this.$store.state.selectedStations[9], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[9].station][index]];


           this.chart10.title.text = Object.is(this.$store.state.selectedStations[9], undefined) ? null :
           this.$store.state.selectedStations[9].station;


           this.chart11.series[i].data = Object.is(this.$store.state.selectedStations[10], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[10].station][index]];

           this.chart11.title.text = Object.is(this.$store.state.selectedStations[10], undefined) ? null :
           this.$store.state.selectedStations[10].station;


           this.chart12.series[i].data = Object.is(this.$store.state.selectedStations[11], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[11].station][index]];


          this.chart12.title.text = Object.is(this.$store.state.selectedStations[11], undefined) ? null :
           this.$store.state.selectedStations[11].station;

//new code for Freke

           this.chart13.series[i].data = Object.is(this.$store.state.selectedStations[12], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[12].station][index]];

           this.chart13.title.text = Object.is(this.$store.state.selectedStations[12], undefined) ? null :
           this.$store.state.selectedStations[12].station;


          this.chart14.series[i].data = Object.is(this.$store.state.selectedStations[13], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[13].station][index]];

           this.chart14.title.text = Object.is(this.$store.state.selectedStations[13], undefined) ? null :
           this.$store.state.selectedStations[13].station;


           this.chart15.series[i].data = Object.is(this.$store.state.selectedStations[14], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[14].station][index]];

           this.chart15.title.text = Object.is(this.$store.state.selectedStations[14], undefined) ? null :
           this.$store.state.selectedStations[14].station;


           this.chart16.series[i].data = Object.is(this.$store.state.selectedStations[15], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[15].station][index]];

           this.chart16.title.text = Object.is(this.$store.state.selectedStations[15], undefined) ? null :
           this.$store.state.selectedStations[15].station;


           this.chart17.series[i].data = Object.is(this.$store.state.selectedStations[16], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[16].station][index]];


           this.chart17.title.text = Object.is(this.$store.state.selectedStations[16], undefined) ? null :
           this.$store.state.selectedStations[16].station;


           this.chart18.series[i].data = Object.is(this.$store.state.selectedStations[17], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[17].station][index]];

           this.chart18.title.text = Object.is(this.$store.state.selectedStations[17], undefined) ? null :
           this.$store.state.selectedStations[17].station;


           this.chart19.series[i].data = Object.is(this.$store.state.selectedStations[18], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[18].station][index]];


          this.chart19.title.text = Object.is(this.$store.state.selectedStations[18], undefined) ? null :
           this.$store.state.selectedStations[18].station;


           this.chart20.series[i].data = Object.is(this.$store.state.selectedStations[19], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[19].station][index]];

           this.chart20.title.text = Object.is(this.$store.state.selectedStations[19], undefined) ? null :
           this.$store.state.selectedStations[19].station;


           this.chart21.series[i].data = Object.is(this.$store.state.selectedStations[20], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[20].station][index]];

           this.chart21.title.text = Object.is(this.$store.state.selectedStations[20], undefined) ? null :
           this.$store.state.selectedStations[20].station;


           this.chart22.series[i].data = Object.is(this.$store.state.selectedStations[21], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[21].station][index]];

           this.chart22.title.text = Object.is(this.$store.state.selectedStations[21], undefined) ? null :
           this.$store.state.selectedStations[21].station;


           this.chart23.series[i].data = Object.is(this.$store.state.selectedStations[22], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[22].station][index]];


           this.chart23.title.text = Object.is(this.$store.state.selectedStations[22], undefined) ? null :
           this.$store.state.selectedStations[22].station;


           this.chart24.series[i].data = Object.is(this.$store.state.selectedStations[23], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[23].station][index]];

           this.chart24.title.text = Object.is(this.$store.state.selectedStations[23], undefined) ? null :
           this.$store.state.selectedStations[23].station;


           this.chart25.series[i].data = Object.is(this.$store.state.selectedStations[24], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[24].station][index]];


          this.chart25.title.text = Object.is(this.$store.state.selectedStations[24], undefined) ? null :
           this.$store.state.selectedStations[24].station;


          this.chart26.series[i].data = Object.is(this.$store.state.selectedStations[25], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[25].station][index]];

           this.chart26.title.text = Object.is(this.$store.state.selectedStations[25], undefined) ? null :
           this.$store.state.selectedStations[25].station;


           this.chart27.series[i].data = Object.is(this.$store.state.selectedStations[26], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[26].station][index]];

           this.chart27.title.text = Object.is(this.$store.state.selectedStations[26], undefined) ? null :
           this.$store.state.selectedStations[26].station;


           this.chart28.series[i].data = Object.is(this.$store.state.selectedStations[27], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[27].station][index]];


           this.chart28.title.text = Object.is(this.$store.state.selectedStations[27], undefined) ? null :
           this.$store.state.selectedStations[27].station;


           this.chart29.series[i].data = Object.is(this.$store.state.selectedStations[28], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[28].station][index]];

           this.chart29.title.text = Object.is(this.$store.state.selectedStations[28], undefined) ? null :
           this.$store.state.selectedStations[28].station;


           this.chart30.series[i].data = Object.is(this.$store.state.selectedStations[29], undefined) ? null :
           [mnArr[this.$store.state.selectedStations[29].station][index]];


          this.chart30.title.text = Object.is(this.$store.state.selectedStations[29], undefined) ? null :
           this.$store.state.selectedStations[29].station;


        });

      
},
    },

computed:{
  stations() {
      return this.$store.getters.getStations;
    },
    selectedStations() {
      return this.$store.getters.getSelectedStations;
    },
  },
}



</script>

<style src="vue-multiselect/dist/vue-multiselect.min.css"></style>


 <!-- <v-row row-wrap no-gutters>
      <v-col xs5 sm5>
            <Rose v-bind:options="chart7" v-if="renderComponent"/>
      </v-col>
      <v-col xs5 sm5>
            <Rose v-bind:options="chart8" v-if="renderComponent"/>
      </v-col>
            <v-col xs5 sm5>
            <Rose v-bind:options="chart9" v-if="renderComponent"/>
      </v-col>
            <v-col xs5 sm5>
            <Rose v-bind:options="chart10" v-if="renderComponent"/>
      </v-col>
      <v-col xs5 sm5>
            <Rose v-bind:options="chart11" v-if="renderComponent"/>
      </v-col>
            <v-col xs5 sm5>
            <Rose v-bind:options="chart12" v-if="renderComponent"/>
      </v-col>
    </v-row>

-->

    this.chart1.series[1].data = [this.$store.state.selectedcities[0].value[0]];
    this.chart1.series[2].data = [this.$store.state.selectedcities[0].value[1]];
    this.chart1.series[3].data = [this.$store.state.selectedcities[0].value[2]];
    this.chart1.series[5].data = [this.$store.state.selectedcities[0].value[4]];
    this.chart1.series[7].data = [this.$store.state.selectedcities[0].value[6]];
    this.chart1.series[8].data = [this.$store.state.selectedcities[0].value[7]];
    this.chart1.series[9].data = [this.$store.state.selectedcities[0].value[8]];
    this.chart1.series[11].data = [this.$store.state.selectedcities[0].value[10]];
    this.chart1.series[12].data = [this.$store.state.selectedcities[0].value[11]];
    this.chart1.series[13].data = [this.$store.state.selectedcities[0].value[12]];
    this.chart1.series[14].data = [this.$store.state.selectedcities[0].value[13]];
    this.chart1.series[16].data = [this.$store.state.selectedcities[0].value[15]];
    this.chart1.series[17].data = [this.$store.state.selectedcities[0].value[16]];
    this.chart1.series[18].data = [this.$store.state.selectedcities[0].value[17]];
    this.chart1.series[20].data = [this.$store.state.selectedcities[0].value[19]];
    this.chart1.series[21].data = [this.$store.state.selectedcities[0].value[20]];
    this.chart1.series[22].data = [this.$store.state.selectedcities[0].value[21]];




    this.chart1.series[1].data = [this.$store.state.selectedStations[0].value[0]];
    this.chart1.series[2].data = [this.$store.state.selectedStations[0].value[1]];
    this.chart1.series[3].data = [this.$store.state.selectedStations[0].value[2]];
    this.chart1.series[5].data = [this.$store.state.selectedStations[0].value[3]];
    this.chart1.series[8].data = [this.$store.state.selectedStations[0].value[4]];
    this.chart1.series[9].data = [this.$store.state.selectedStations[0].value[5]];
    this.chart1.series[10].data = [this.$store.state.selectedStations[0].value[6]];
    this.chart1.series[11].data = [this.$store.state.selectedStations[0].value[7]];
    this.chart1.series[12].data = [this.$store.state.selectedStations[0].value[8]];
    this.chart1.series[13].data = [this.$store.state.selectedStations[0].value[9]];
    this.chart1.series[14].data = [this.$store.state.selectedStations[0].value[10]];
    this.chart1.series[16].data = [this.$store.state.selectedStations[0].value[11]];
    this.chart1.series[17].data = [this.$store.state.selectedStations[0].value[12]];
    this.chart1.series[18].data = [this.$store.state.selectedStations[0].value[13]];
    this.chart1.series[20].data = [this.$store.state.selectedStations[0].value[14]];
    this.chart1.series[21].data = [this.$store.state.selectedStations[0].value[15]];
    this.chart1.series[22].data = [this.$store.state.selectedStations[0].value[16]];



    n.forEach((element, index) => {
  console.log(element, index);
});


 var seriesdata1 = [
          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 0,
            pointRange: 0.5
          },
          {
            data: [],
            type: "column",
            color: "#1DACE8",
            name: "S_N_AT_Park_tot",
            pointStart: 0,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#1DACE8",
            name: "S_N_CA_Park_tot",
            pointStart: 25.71,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#1DACE8",
            name: "S_N_BTM",
            pointStart: 51.42,
            pointRange: 25.71
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 77.13,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#AEA8A8",
            name: "S_N_TR",
            pointStart: 77.13,
            pointRange: 25.71
          },
          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 102.84,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#F24D29",
            name: "S_P_DE",
            pointStart: 102.84,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#F24D29",
            name: "S_P_DI",
            pointStart: 128.55,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#F24D29",
            name: "S_P_DG",
            pointStart: 154.26,
            pointRange: 25.74
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 180,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_sec",

            pointStart: 180,
            pointRange: 19.2825
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_tert",
            pointStart: 199.2825,
            pointRange: 19.2825
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_work",
            pointStart: 218.465,
            pointRange: 19.2825
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_other",
            pointStart: 237.6475,
            pointRange: 19.2825
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 256.83,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#C4CFD0",
            name: "S_PP_RID_orig",
            pointStart: 256.83,
            pointRange: 8.57
          },

          {
            data: [],
            type: "column",
            color: "#C4CFD0",
            name: "S_PP_RID_rid",
            pointStart: 265.4,
            pointRange: 8.57
          },

          {
            data: [],
            type: "column",
            color: "#C4CFD0",
            name: "S_PP_RID_dest",
            pointStart: 273.97,
            pointRange: 8.57
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 282.54,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#76A08A",
            name: "S_PP_EFF_walk",
            pointStart: 282.54,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#76A08A",
            name: "S_PP_EFF_bike",
            pointStart: 308.25,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#76A08A",
            name: "S_PP_EFF_far",
            pointStart: 333.96,
            pointRange: 26.04
          }]
    var seriesdata2 = [
          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 0,
            pointRange: 0.5
          },
          {
            data: [],
            type: "column",
            color: "#1DACE8",
            name: "S_N_AT_Park_tot",
            pointStart: 0,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#1DACE8",
            name: "S_N_CA_Park_tot",
            pointStart: 25.71,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#1DACE8",
            name: "S_N_BTM",
            pointStart: 51.42,
            pointRange: 25.71
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 77.13,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#AEA8A8",
            name: "S_N_TR",
            pointStart: 77.13,
            pointRange: 25.71
          },
          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 102.84,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#F24D29",
            name: "S_P_DE",
            pointStart: 102.84,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#F24D29",
            name: "S_P_DI",
            pointStart: 128.55,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#F24D29",
            name: "S_P_DG",
            pointStart: 154.26,
            pointRange: 25.74
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 180,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_sec",

            pointStart: 180,
            pointRange: 19.2825
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_tert",
            pointStart: 199.2825,
            pointRange: 19.2825
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_work",
            pointStart: 218.465,
            pointRange: 19.2825
          },

          {
            data: [],
            type: "column",
            color: "#EDCB64",
            name: "S_PP_MOT_other",
            pointStart: 237.6475,
            pointRange: 19.2825
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 256.83,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#C4CFD0",
            name: "S_PP_RID_orig",
            pointStart: 256.83,
            pointRange: 8.57
          },

          {
            data: [],
            type: "column",
            color: "#C4CFD0",
            name: "S_PP_RID_rid",
            pointStart: 265.4,
            pointRange: 8.57
          },

          {
            data: [],
            type: "column",
            color: "#C4CFD0",
            name: "S_PP_RID_dest",
            pointStart: 273.97,
            pointRange: 8.57
          },

          {
            data: [10],
            type: "column",
            color: "#AEA8A8",
            name: "TESTMARK",
            pointStart: 282.54,
            pointRange: 0.5
          },

          {
            data: [],
            type: "column",
            color: "#76A08A",
            name: "S_PP_EFF_walk",
            pointStart: 282.54,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#76A08A",
            name: "S_PP_EFF_bike",
            pointStart: 308.25,
            pointRange: 25.71
          },

          {
            data: [],
            type: "column",
            color: "#76A08A",
            name: "S_PP_EFF_far",
            pointStart: 333.96,
            pointRange: 26.04
          }]




                      
            //Send Computed Array to Store
            //this.$store.commit('setComputedArray', mnArr)

          //console.log(this.$store.computedarray[0])


          //  objlen.forEach(k => {
          //  var newValue = ((this.$store.state.selectedStations[i].value[k] * 10) / maxi)
          //  newArr.push(newValue)
          //  });
          //  mnArr["obj" + i] = newArr
          //  console.log(mnArr)
       // });

       // this.chart1.series[1].data = [mnArr["obj0"][0]]



        // seq.forEach(i => {
        //    objlen.forEach(k => {
        //       arr.push(this.$store.state.selectedStations[i].value[k])
        //    })
            //do something
        // });

         //var maxi = Math.max(...arr)
        // console.log(Math.max(arr))

         //seq.forEach(i => {
         // var newValue = ((this.$store.state.selectedStations[i].value[1] * 10) / maxi)
         //   newArr.push(newValue)
        // });

        
     //  console.log(arr)
     //  console.log(newArr[0])




        //  this.chart1.series[1].data = Object.is(this.$store.state.selectedStations[0], undefined) ? null :
        //  [this.$store.state.computedarray[0][0]]
          //Object.is(this.$store.state.selectedStations[0], undefined) ? null :
    
          //[this.$store.state.selectedStations[0].value[index]];

//          this.chart2.series[i].data = Object.is(this.$store.state.selectedStations[1], undefined) ? null :
//          [this.$store.state.selectedStations[1].value[index]];

//          this.chart3.series[i].data = Object.is(this.$store.state.selectedStations[2], undefined) ? null :
//          [this.$store.state.selectedStations[2].value[index]];
          
//          this.chart4.series[i].data = Object.is(this.$store.state.selectedStations[3], undefined) ? null :
//          [this.$store.state.selectedStations[3].value[index]];

//          this.chart5.series[i].data = Object.is(this.$store.state.selectedStations[4], undefined) ? null :
//          [this.$store.state.selectedStations[4].value[index]];

//          this.chart6.series[i].data = Object.is(this.$store.state.selectedStations[5], undefined) ? null :
//          [this.$store.state.selectedStations[5].value[index]];


  <!--  <div>
        <v-autocomplete
          @input="triggerSetSelectedStations "
          :items= "$store.state.stations"
          item-text= "station"
          :value="selectedStations"
          label="Select"
          hint="Vuex select"
          return-object
          color="green"
          item-color="green"
          single-line
          multiple
          chips
        >
        <v-chip color="red"></v-chip>
        </v-autocomplete>
    </div> -->


    {data: [10]}, {data: []}, {data: []}, {data: []}, {data: [10]}, {data: []}, {data: [10]},
          {data: []}, {data: []}, {data: []}, {data: [10]},{data: []}, {data: []},{data: []}, {data: []},
          {data: [10]},{data: []},{data: []},{data: []},{data: [10]},{data: []}, {data: []}, {data: []}]