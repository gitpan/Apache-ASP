<% 
	use DemoASP; 
	$demo = &DemoASP::new($Request);
%>
<html>
<head><title><%=$demo->{title}%></title></head>
<body bgcolor=<%=$demo->{bgcolor}%>>

<center>
<table border=1>
<tr><th colspan=2>Server Variables / CGI Environment</th></tr>

<!--	
	Please note that under normal ASP, Collections, as objects
	like ServerVariables are referred to, do not have iterators
	through their members.  Being able to do this is not 
	something that PerlScript nor VBScript support, so 
	use the iterators at your own risk!
-->

<% for(sort keys %{$Request->ServerVariables()}) { %>
	<tr>
		<td><tt><%=$_%></tt></td>
		<td><tt><%=$Request->ServerVariables($_)%></tt></td>
	</tr>
<% } %>
</table>
</center>
<a href="source.asp?file=<%=$Request->ServerVariables("SCRIPT_NAME")%>">
view this file's source
</a>
</body>
</html>
