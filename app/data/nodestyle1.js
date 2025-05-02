[
  {"selector":"node", "css": {
    "border-width": "1px",
    "content": "data(id)",
	"background-color": "gray",
	"background-fit": "cover cover",
	"shape" : "rectangle",
	"border-style": "data(borderstyle)",
	"background-image": "data(path)",
	"label" : "data(shared_name)",
	"height": "data(Height)",
	"z-index" : 1,
	"width": "data(Width)",
	"background-fit": "cover cover",
	"font-size" : "data(Label_size)",
	"border-color": "green"
  }
  },
  {"selector":"node:selected", "css": {
	"background-color": "white",
	"width": 700,
	"height": 700,
	"background-image": "data(path)",
	"background-fit": "contain",
	"z-index" : 4,
	"font-size" : 50
	
  }
  },
  {"selector": "node[NumChildren<=1]", "css": {
        "height": "10",
	"width": "10"
        }},
  {"selector": "edge", "css": {
    "line-color": "gray",
	"label": "data(label)"
  }},
  {
    "selector": "node.significant", 
    "css": {
      "border-width": "5px",  
      "border-style": "solid",
	  "border-color": "green"
    }
  }
]