(function(global){
  "use strict";
  global.MSLipidMapperCyDefaultStyle = function(){
    return [
      { selector: "node", style: {
          "border-width": "data(BorderWidth)",
          "border-color": "data(Color)",
          "border-style": "data(borderstyle)",
          "label": "data(label)",
          "background-color": "white",
          "shape": "round-rectangle",
          "background-image": "data(path)",
          "background-image-crossorigin": "anonymous",
          "background-fit": "cover cover",
          "height": "data(Height)",
          "width": "data(Width)",
          "font-size": "data(Label_size)",
          "text-valign": "top",
          "text-halign": "center",
          "text-margin-y": 22
      }},
      { selector: "node[IsJunction = 1]", style: {
          "shape": "ellipse",
          "width": 24,
          "height": 24,
          "label": "",
          "border-width": 2,
          "border-color": "#666",
          "background-color": "white",
          "background-image": "none"
      }},
      { selector: "node:selected", style: { "border-width": 3 } },
      { selector: "edge", style: {
          "width": 2,
          "line-color": "#888",
          "curve-style": "bezier",
          "target-arrow-shape": "triangle",
          "target-arrow-color": "#888",
          "arrow-scale": 1.5
      }},
      { selector: 'edge[edgeType = "toJunction"]', style: {
          "target-arrow-shape": "none"
      }}
    ];
  };
})(window);
