<template>
  <v-app class="grey lighten-4 background: #FF44FF; ">
    <Navbar/>
    <v-content>
     <vue-page-transition name="overlay-right">
     <router-view></router-view>
     </vue-page-transition>
    </v-content>
  </v-app>
</template>

<script>

import Navbar from './components/Navbar'

export default {
  components: { Navbar },
  name: 'App',
data() {
    return {
    }
   }
  }
</script>
<style>
table.v-table tbody td {
    font-size: 18px !important;
}
   .overlay-left,  .overlay-right,  .overlay-top,  .overlay-bottom   {
    background: #EDCB64 !important;
}
</style>