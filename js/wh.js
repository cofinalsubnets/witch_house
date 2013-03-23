$(document).ready(function() {
  window.ws = createSocket();
  window.msg_count = 0;

  window.message_view = Backbone.View.extend({
    tagName: "div",
    template: function(t) {return t;},
    render: function(t) {
      this.$el.html(this.template(t));
      this.$el.addClass("wh_msg");
      if((msg_count % 2) == 0) {
        this.$el.addClass("even");
      } else {
        this.$el.addClass("odd");
      }
      return this;
    }
  });

  window.message_list_view = Backbone.View.extend({
    el: $("#wh_box"),
    show_message: function(txt) {
      msg_count += 1;
      this.$el.append((new message_view).render(txt).$el);
      var cs = this.$el.children();
      if(cs.toArray().length > 30) cs.first().remove();
      this.$el.scrollTop(1000);
    }
  });

  window.messages = new message_list_view;

  $("#wh_input_box").keypress(function(e) {
    var txt = $("#wh_input_box").val();
    if(e.keyCode == 13 && txt != "") {
      e.preventDefault();
      ws.send(txt);
      messages.show_message("> " + txt);
      $("#wh_input_box").val('');
    }
  });

});

function createSocket() {
  ws_url = "ws://ec2-54-244-180-87.us-west-2.compute.amazonaws.com:8888"
  var s = new WebSocket(ws_url);
  s.onmessage = function (msg) { messages.show_message(msg.data.replace(/\n/g,"<br />")); };
  s.onclose = function (msg) { messages.show_message("### connection closed ###"); };
  return s;
}




