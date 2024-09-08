[
  {"selector":"node", "css": {
    "border-width": "1px",
    "content": "data(id)",
	"background-color": "data(color)",
	"background-fit": "cover cover",
	"shape" : "rectangle",
	"background-image": "data(path)",
	"label" : "data(shared_name)",
	"height": "data(Height)",
	"z-index" :  1,
	"width": "data(Width)",
	"background-fit": "cover cover",
	"font-size" : "data(Label_size)"
  }
  },
  {"selector":"node:selected", "css": {
	  "overlay-color": "white",
	  "border-width": "5px",
	"overlay-opacity": 0.1,
	"z-index" : 4
  }
  },
  {"selector": "node[NumChildren<=1]", "css": {
        "height": "10",
	"width": "10"
        }},
  {"selector": "edge", "css": {
    "line-color": "gray",
	"label": "data(label)"
  }}
]