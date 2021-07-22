import Vue from 'vue'
import Router from 'vue-router'
import Home from './views/Home.vue'

Vue.use(Router)

export default new Router({
  mode: 'history',
  base: process.env.BASE_URL,
  routes: [
    {
      path: '/',
      name: 'home',
      component: Home
    },
    {
      path: '/about',
      name: 'about',
      // route level code-splitting
      // this generates a separate chunk (about.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "about" */ './views/About.vue')
    },
    {
      path: '/terms',
      name: 'terms',
      // route level code-splitting
      // this generates a separate chunk (about.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "about" */ './views/Terms.vue')
    },

    {
      path: '/indicatoren',
      name: 'indicatoren',
      // route level code-splitting
      // this generates a separate chunk (nodata.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "nodata" */ './components/Indicatoren.vue')
    },
    {
      path: '/metadata',
      name: 'metadata',
      // route level code-splitting
      // this generates a separate chunk (nodata.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "nodata" */ './components/metadata.vue')
    },
    {
      path: '/woordjeuitleg',
      name: 'woordjeuitleg',
      // route level code-splitting
      // this generates a separate chunk (nodata.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "nodata" */ './components/diagram_text.vue')
    },
     {
      path: '/kaarten',
      name: 'kaarten',
      // route level code-splitting
      // this generates a separate chunk (nodata.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "nodata" */ './components/maps.vue')
    },
    {
      path: '/tabledata',
      name: 'tabledata',
      // route level code-splitting
      // this generates a separate chunk (nodata.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "nodata" */ './components/tabledata.vue')
    },
    {
      path: '/radardiagrammen',
      name: 'radardiagrammen',
      // route level code-splitting
      // this generates a separate chunk (nodata.[hash].js) for this route
      // which is lazy-loaded when the route is visited.
      component: () => import(/* webpackChunkName: "nodata" */ './components/rose_diagram.vue')
    }
  ]
})
