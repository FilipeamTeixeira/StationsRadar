<template>
<!-- <nav> -->
      <v-app-bar flat app class='elevation-0'> 
     
    <!--  <v-app-bar-nav-icon @click="drawer = !drawer"></v-app-bar-nav-icon> -->
      <v-toolbar-title class="text grey--text">
        <span class="font-weight-light">Stations</span>
        <span>Radar</span>
      </v-toolbar-title>

<!-- New Code
<router-link to="/vuex" active-class="active"> Test </router-link>
-->

<div class="flex-grow-1"></div>
   <v-toolbar-items align-center>

         <v-btn v-for="link in links"
         :key="link.text"
         router :to="link.route"
         v-on:click="makeActive(link.color)"
         text
         class="active blue-grey--text"
         exact
         small
         tile
         v-bind:style="`border-bottom-color:${active===link.color?active:''}`"
         >{{link.text}}</v-btn>

      </v-toolbar-items>
      <v-spacer></v-spacer>
      <v-spacer></v-spacer>
      <v-spacer></v-spacer>


<!--    <v-spacer></v-spacer>
      <v-flex>
      <v-img :src="require('../assets/railacc_icon.png')" class="my-3" position="center right" contain height="50"></v-img>
      </v-flex>
-->
      <v-avatar size="50px" class="ms-12">
      <v-img :src="require('../assets/railacc_icon.png')" position="center right"></v-img>
      </v-avatar>
      </v-app-bar>

 <!--   <v-navigation-drawer dark app v-model="drawer" temporary>
      <v-list>
         <v-list-item
          v-for="link in links"
          :key="link.text"
          router :to="link.route"
        >
        <v-list-item-icon>
            <v-icon class="white--text">{{ link.icon }}</v-icon>
          </v-list-item-icon>
          <v-list-item-content>
            <v-list-item-title class="white--text">{{ link.text }}</v-list-item-title>
          </v-list-item-content>
        </v-list-item>
      </v-list>
    </v-navigation-drawer> -->
 <!-- </nav> -->
</template>

<script>
export default {
  data() {
    return {
      active: "red",
      options: {
        customProperties: true
      },
      drawer: false,
      links: [
        { icon: 'dashboard', text: 'Home', route: '/' , color:"rgb(215,219,220)"},
        { icon: 'mdi-chart-pie', text: 'Radar diagrammen', route: '/radardiagrammen' , color:"#76A08A"},
        { icon: 'mdi-chart-timeline-variant', text: 'Indicatoren', route: '/indicatoren' , color:"rgb(29,172,232)"},
        { icon: 'mdi-map-marker', text: 'Kaarten', route: '/kaarten', color:"#F24D29"},
        { icon: 'mdi-table-large', text: 'Tabel', route: '/tabledata', color:"#AEA8A8"},
        { icon: 'mdi-database-import', text: 'Metadata', route: '/metadata', color:"#B62A3D"},
        { icon: 'mdi-database-import', text: 'Woordje uitleg', route: '/woordjeuitleg', color:"#7496D2"},
        { icon: 'folder', text: 'About', route: '/About', color:"#2E604A"},
        { icon: 'mdi-database-import', text: 'Terms', route: '/terms', color:"#EDCB64"},
      ]
    }
  },
  methods: {
    makeActive: function(link) {
      this.active = link;
    },
  }
}
</script>

<style lang="scss">

.v-btn:hover:before {background-color:transparent !important}
.v-btn:focus:before {background-color:transparent !important}


.active{
  border-bottom:transparent solid 3px;
}

</style>
