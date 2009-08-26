$header(title="New show")$

<form action="." method="post">
    <label for="name">Name</label>
    <input type="text" name="name" value="" />

    <label for="description">Description</label>
    <textarea name="description"></textarea>

    <label for="url">Url</label>
    <input type="text" name="url" value="" />

    <input type="submit" value="Create" />
    <a href="." class="cancel">Cancel</a>
</form>

$footer()$

