require('!!file-loader?name=[name].[ext]!./index.html')

let ReactDOM = require('react-dom')
let React = require("react")
let createReactClass = require('create-react-class')
let Qs = require('qs')
let Cookie = require('cookie')
let XMLHttpRequest = require("xhr2")

require('./tuto.webflow/css/orders.css');
require('./tuto.webflow/css/details.css');

let routes = {
  "orders": {
    path: (params) => {
      return "/";
    },
    match: (path) => {
      return (path == "/") && { handlerPath: [Layout, Header, Orders] } // Note that we use the "&&" expression to simulate a IF statement
    }
  },
  "order": {
    path: (params) => {
      return "/order/" + params;
    },
    match: (path, qs) => {
      var r = new RegExp("/order/([^/]*)$").exec(path)
      return r && { handlerPath: [Layout, Header, Order], order_id: r[1] } // Note that we use the "&&" expression to simulate a IF statement
    }
  }
}

let HTTP = new (function () {
  this.get = (url) => this.req('GET', url)
  this.delete = (url) => this.req('DELETE', url)
  this.post = (url, data) => this.req('POST', url, data)
  this.put = (url, data) => this.req('PUT', url, data)

  this.req = (method, url, data) => new Promise((resolve, reject) => {
    var req = new XMLHttpRequest()
    req.open(method, url)
    req.responseType = "text"
    req.setRequestHeader("accept", "application/json,*/*;0.8")
    req.setRequestHeader("content-type", "application/json")
    req.onload = () => {
      if (req.status >= 200 && req.status < 300) {
        resolve(req.responseText && req.responseText)
      } else {
        reject({ http_code: req.status })
      }
    }
    req.onerror = (err) => {
      reject({ http_code: req.status })
    }
    req.send(data && JSON.stringify(data))
  })
})()

let remoteProps = {
  user: (props) => {
    return {
      url: "/api/me",
      prop: "user"
    }
  },
  orders: (props) => {
    if (!props.user) return
    var qs = { ...props.qs, user_id: props.user.value.id }
    var query = Qs.stringify(qs)
    return {
      url: "/api/orders" + (query == '' ? '' : '?' + query),
      prop: "orders"
    }
  },
  order: (props) => {
    return {
      url: "/get_values/" + props.order_id,
      prop: "order"
    }
  }
}

let Child = createReactClass({
  render() {
    var [ChildHandler, ...rest] = this.props.handlerPath
    return <ChildHandler {...this.props} handlerPath={rest} />
  }
})

let Layout = createReactClass({
  render() {
    return <JSXZ in="orders" sel=".layout">
      <Z sel=".container">
        <this.props.Child {...this.props} />
      </Z>
    </JSXZ>
  }
})

let ErrorPage = createReactClass({
  render() {
    return <div>{this.props.message}</div>
  }
})

let Header = createReactClass({
  render() {
    return <JSXZ in="orders" sel=".headers">
      <Z sel=".headers-container">
        <this.props.Child {...this.props} />
      </Z>
    </JSXZ>
  }
})

let Orders = createReactClass({
  statics: {
    remoteProps: [remoteProps.orders]
  },
  render() {
    console.log(this.props.Child)
    return <JSXZ in="orders" sel=".orders">
      <Z sel=".orders-container">
      </Z>
    </JSXZ>
  }
})

let Order = createReactClass({
  render() {
    return <JSXZ in="detail" sel=".container">
      <Z sel=".container-role">
      </Z>
    </JSXZ>
  }
})

function addRemoteProps(props) {
  return new Promise((resolve, reject) => {
    let remoteProps = Array.prototype.concat.apply([],
      props.handlerPath
        .map((c) => c.remoteProps) // -> [[remoteProps.orders], null]
        .filter((p) => p) // -> [[remoteProps.orders]]
    )

    remoteProps = remoteProps
      .map((spec_fun) => spec_fun(props)) // [{url: '/api/orders', prop: 'orders'}]
      .filter((specs) => specs) // get rid of undefined from remoteProps that don't match their dependencies
      .filter((specs) => !props[specs.prop] || props[specs.prop].url != specs.url) // get rid of remoteProps already resolved with the url
    if (remoteProps.length == 0)
      return resolve(props)


    // All remoteProps can be queried in parallel. This is just the function definition, see its use below.
    const promise_mapper = (spec) => {
      // we want to keep the url in the value resolved by the promise here : spec = {url: '/api/orders', value: ORDERS, prop: 'orders'}
      return HTTP.get(spec.url).then((res) => { spec.value = res; return spec })
    }

    const reducer = (acc, spec) => {
      // spec = url: '/api/orders', value: ORDERS, prop: 'user'}
      acc[spec.prop] = { url: spec.url, value: spec.value }
      return acc
    }

    const promise_array = remoteProps.map(promise_mapper)
    return Promise.all(promise_array)
      .then(xs => xs.reduce(reducer, props), reject)
      .then((p) => {
        // recursively call remote props, because props computed from
        // previous queries can give the missing data/props necessary
        // to define another query
        return addRemoteProps(p).then(resolve, reject)
      }, reject)
  })
}

let GoTo = (route, params, query) => {
  var qs = Qs.stringify(query)
  var url = routes[route].path(params) + ((qs == '') ? '' : ('?' + qs))
  history.pushState({}, "", url)
  return onPathChange()
}

let browserState = { Child: Child }

function onPathChange() {
  var path = location.pathname
  var qs = Qs.parse(location.search.slice(1))
  var cookies = Cookie.parse(document.cookie)

  browserState = {
    ...browserState,
    path: path,
    qs: qs,
    cookie: cookies
  }
  let route

  for (var key in routes) {
    routeProps = routes[key].match(path, qs)
    if (routeProps) {
      route = key
      break;
    }
  }

  if (!route) return ReactDOM.render(<ErrorPage message={"Not Found"} code={404} />, document.getElementById('root'))

  browserState = {
    ...browserState,
    ...routeProps,
    route: route
  }
  //console.log("browserState in onPathChange", browserState)
  addRemoteProps(browserState).then(
    (props) => {
      browserState = props
      //console.log(props)
      // Log our new browserState
      console.log("browserState in promise resolved", browserState)
      // Render our components using our remote data
      ReactDOM.render(<Child {...browserState} />, document.getElementById('root'))
    }, (res) => {
      ReactDOM.render(<ErrorPage message={"Shit happened"} code={res.http_code} />, document.getElementById('root'))
    })
}

window.addEventListener("popstate", () => { onPathChange() })
onPathChange() // We also call onPathChange once when the js is loaded