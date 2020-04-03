var mapElement;
var layerobjects = {};
var currentLayer = '';
$(function () {
  $('[data-toggle="tooltip"]').tooltip();
});

var loadHandler = function (event) {
      const keys = Object.keys(layerobjects);
      for (var  key of keys) {
        if(key !== event.sourceTarget.options.layerId){
            mapElement.removeLayer(layerobjects[key]);  
            delete layerobjects[key];
        }

      }
};
 
 
function onLayerAddFunction(e){
  if( typeof(e.layer.options.layers)!='undefined'  ) {  
      layerobjects[e.layer.options.layerId]=e.layer; 
          e.layer.on('load', loadHandler); 
  } 
 
}
 