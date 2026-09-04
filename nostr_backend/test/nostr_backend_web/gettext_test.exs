defmodule NostrBackendWeb.GettextTest do
  use ExUnit.Case, async: true
  use Gettext, backend: NostrBackendWeb.Gettext

  test "gettext macros resolve through the backend" do
    assert gettext("close") == "close"
    assert Gettext.dgettext(NostrBackendWeb.Gettext, "errors", "is invalid") == "is invalid"
  end
end
