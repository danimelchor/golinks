let show_form r =
  <html>
  <body>
    <form method="POST" action="/">
      <%s! Dream.csrf_tag r %>
      <input name="name">
      <input name="url">
      <button type="submit">Go</button>
    </form>
  </body>
  </html>
