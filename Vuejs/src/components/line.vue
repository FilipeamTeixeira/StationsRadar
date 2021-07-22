<template>
<highcharts :options="lineGraph"></highcharts>
</template>

<script>

import car from "highcharts-vue";
import Highcharts from "highcharts";

import parallelCoordinates from "highcharts/modules/parallel-coordinates";

parallelCoordinates(Highcharts);


export default {
    props:["options"],
    components:{
        lineGraph: car
    },
    watch: {
    options: {
      handler(newOpt_line) {
        this.lineGraph.yAxis[0].categories = newOpt_line.yAxis[0].categories
        this.lineGraph.series = newOpt_line.series
        this.lineGraph.xAxis.categories = newOpt_line.xAxis.categories
      },
      deep: true
    }
  },
    data () {
        return{
            lineGraph: Highcharts.merge(this.options,{
    chart: {
        type: 'spline',
        parallelCoordinates: true,
        parallelAxes: {
            lineWidth: 1
        },
        marginLeft: 10,
        height: (9 / 16 * 100) + '%' // 16:9 ratio
    },
        credits: {
          enabled: false
        },
    title: {
        text: null
    },
    tooltip: {
    formatter: function() {
        if (this.key != ""){
        return this.y;
        }
        return ""
    }
},
    plotOptions: {
                series: {
                    lineWidth: 1,
                    color: 'rgb(29,172,232, .5)',
                    marker: {
                    symbol: 'circle',
                    radius: 0
                    }}},
    xAxis: {
        categories: [
        ],
        labels: {
            style: {
                fontWeight: 'bold'
            }
        },
    },
    yAxis: [{
        categories: [],
        lineWidth: 0,
        offset: 10,
        labels: {
            align: 'right'
        },
        name:'Cities'
    },{
        lineWidth: 1,
        labels: false,
    },{
        lineWidth: 1,
        labels: false
    }, {
        lineWidth: 1,
        labels: false
    }, {
        lineWidth: 1,
        labels: false
    }, {
        lineWidth: 1,
        labels: false
    }, {
        lineWidth: 1,
        labels: false
    }, {
        lineWidth: 1,
        labels: false
    }],
series: [],
    colors: ['#2196F3']
            })
    }
},

}




</script>

