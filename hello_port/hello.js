require('node_erlastic').server(function(term,from,acc,done){
    if (term == "hello") return done("reply","wolrd", acc);
    throw new Error("unexpected request")
  });