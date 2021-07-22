import Vue from 'vue'
import App from './App.vue'
import router from './router'
import store from './store'
import vuetify from './plugins/vuetify'
import HighchartsVue from 'highcharts-vue'
import Highcharts from 'highcharts'
import HighchartsMore from 'highcharts/highcharts-more'
import HighchartsNoData from 'highcharts/modules/no-data-to-display'
import VueLocalStorage from 'vue-localstorage';
import Multiselect from 'vue-multiselect'
import VuePageTransition from 'vue-page-transition'
import VueYouTubeEmbed from 'vue-youtube-embed'
import VueAnalytics from 'vue-analytics'



Vue.config.productionTip = false;

const isProd = process.env.NODE_ENV === 'production'

Vue.use(VueAnalytics, {
  id: 'UA-131837766-1',
  require: ['displayfeatures'],
  router,
  debug:{
    enabled: !isProd,
    sendHitTask: isProd
  },
  trackEvent: true
})

Vue.use(HighchartsVue);
HighchartsMore(Highcharts);
HighchartsNoData(Highcharts);
Vue.use(VueLocalStorage);
Vue.component('multiselect', Multiselect)

Vue.use(VuePageTransition)
Vue.use(VueYouTubeEmbed)




new Vue({
  router,
  store,
  vuetify,
  render: h => h(App)
}).$mount('#app')
