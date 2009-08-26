$header(title="Shows")$
    <h1>Shows <small>($length(shows)$ total)</small></h1>
    <table>
    $shows: {show|
        <tr>
            <td>$show.name$</td>
            <td>$show.description$</td>
            <td><a href="$show.url$">$show.url$</a></td>
            <td><a href="/shows/$show.id$">Details</a></td>
        </tr>
    }$
        <tr class='footer'>
            <td colspan="4">
                <a href="new">New</a>
            </td>
        </tr>
    </table>
    <a href="/">Back</a>
$footer()$
