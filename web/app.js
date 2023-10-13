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
    // if (!props.user) return
    // , user_id: props.user.value.id
    var qs = { ...props.qs }
    var query = Qs.stringify(props.qs)
    return {
      url: "/api/orders" + (query == '' ? '' : '?' + query),
      prop: "orders"
    }
  },
  order: (props) => {
    return {
      url: "/api/order/" + props.order_id,
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
  modal(spec) {
    this.setState({
      modal: {
        ...spec, callback: (res) => {
          this.setState({ modal: null }, () => {
            if (spec.callback) spec.callback(res)
          })
        }
      }
    })
  },
  render() {
    let modal_component = {
      'delete': (props) => <DeleteModal {...props} />
    }[this.state.modal && this.state.modal.type];

    modal_component = modal_component && modal_component(this.state.modal)

    return <JSXZ in="orders" sel=".layout">
      <Z sel=".modal-wrapper" className={cn(classNameZ, { 'hidden': !modal_component })}>
        {modal_component}
      </Z>
      <Z sel=".container">
        <this.props.Child {...this.props} />
      </Z>
    </JSXZ>
  }
})

let DeleteModal = createReactClass({
  render() {
    var props = {
      ...this.props, modal: this.modal
    }

    return <JSXZ in="orders" sel=".modal-wrapper">
      <this.props.Child {...this.props} />
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
    this.props.modal({
      type: 'delete',
      title: 'Order deletion',
      message: `Are you sure you want to delete this ?`,
      callback: (value) => {
        //Do something with the return value
      }
    })
    return <JSXZ in="orders" sel=".orders">
      {
        JSON.parse(this.props.orders.value).results.map(order => (<JSXZ in="orders" sel=".list-2" key={order.id}>
          <Z sel=".id-2">{order.id}</Z>
          <Z sel=".name-2">{order.custom.customer.full_name}</Z>
          <Z sel=".adresse-2">{order.custom.billing_address.street}</Z>
          <Z sel=".items-2">{order.custom.items.length}</Z>
          <Z sel=".w-button" onClick={(e) => {
            e.preventDefault()
            this.props.Link.GoTo("order", { order_id: order.remoteid, order_data: order }, {})
          }}><ChildrenZ />
          </Z>
          <Z sel=".delete"><ChildrenZ /></Z>
        </JSXZ>))
      }
    </JSXZ>
  }
})

let Order = createReactClass({
  statics: {
    remoteProps: [remoteProps.order]
  },
  render() {
    return <JSXZ in="detail" sel=".container-role">
      {/* <Z sel=".order-client">{JSON.parse(this.props.order.value).data.custom.customer.full_name}</Z>
      <Z sel=".order-adress">{JSON.parse(this.props.order.value).data.custom.billing_address.street}</Z>
      <Z sel=".order-title">{`Commande n° ${JSON.parse(this.props.order.value).id}`}</Z> */}
      {
        JSON.parse(this.props.order.value).data.custom.items.map(item => (
          <JSXZ in="detail" sel=".item-list" key={item.item_id}>
            <Z sel=".product-name">{item.product_title}</Z>
            <Z sel=".quantity">{item.quantity_to_fetch}</Z>
            <Z sel=".unit-price">{item.unit_price}</Z>
            <Z sel=".total-price">{item.price * item.quantity_to_fetch}</Z>
          </JSXZ>))
      }
    </JSXZ>
  }
})

const cn = function () {
  var args = arguments, classes = {}
  for (var i in args) {
    var arg = args[i]
    if (!arg) continue
    if ('string' === typeof arg || 'number' === typeof arg) {
      arg.split(" ").filter((c) => c != "").map((c) => {
        classes[c] = true
      })
    } else if ('object' === typeof arg) {
      for (var key in arg) classes[key] = arg[key]
    }
  }
  return Object.keys(classes).map((k) => classes[k] && k || '').join(' ')
}

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

let Link = {
  GoTo(route, params, query) {
    var qs = Qs.stringify(query)

    var url = routes[route].path(params.order_id) + ((qs == '') ? '' : ('?' + qs))
    history.pushState({}, "", url)
    return onPathChange()
  }
}

let browserState = { Child: Child, Link: Link }

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

      ReactDOM.render(<Child {...browserState} />, document.getElementById('root'))
    }, (res) => {
      ReactDOM.render(<ErrorPage message={"Shit happened"} code={res.http_code} />, document.getElementById('root'))
    })
}

window.addEventListener("popstate", () => { onPathChange() })
onPathChange() // We also call onPathChange once when the js is loaded