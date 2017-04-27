# Development Environment

<table>
  <tr>
    <th>Item</th>
    <th>Name</th>
    <th>Version</th>
    <th>Description</th>
  </tr>
  <tr>
    <td>IDE</td>
    <td>RStudio</td>
    <td>0.98.1091</td>
    <td>--</td>
  </tr>
  <tr>
    <td>Language</td>
    <td>R</td>
    <td>3.1.2</td>
    <td>--</td>
  </tr>
  <tr>
    <td>Web Framework</td>
    <td>Shiny</td>
    <td>0.12.1</td>
    <td>Framework that allows you to create web applications is R.</td>
  </tr>
  <tr>
    <td rowspan="5">Specific packages</td>
    <td>Shinydashboard</td>
    <td>0.3.0</td>
    <td>CSS framework that allows you to create dashboard looking layouts.</td>
  </tr>
  <tr>
    <td>Shinyapps</td>
    <td>0.3.63</td>
    <td>Library that allows you to deploy applications to shinyapps.io directly from RStudio.</td>
  </tr>
  <tr>
    <td>Leaftlet</td>
    <td>1.0.0.9999</td>
    <td>Create interactive maps with the JavaScript ‘Leaflet’ Library</td>
  </tr>
  <tr>
    <td>GoogleVis</td>
    <td>0.5.8</td>
    <td>R interface to Google Charts</td>
  </tr>
  <tr>
    <td>Lubridate</td>
    <td>1.3.3</td>
    <td>Makes dealing with dates easier</td>
  </tr>
  <tr>
    <td>Hosting service</td>
    <td>Shinyapps.io</td>
    <td>--</td>
    <td>Platform as a Service on which you can deploy Shiny applications.</td>
  </tr>
  <tr>
    <td>Github Repository</td>
    <td>https://github.com/ieda-project/dashboard-v2</td>
    <td></td>
    <td></td>
  </tr>
</table>


# Run The Application Locally

1. Clone the repository

2. Open RStudio

3. Set the working directory to the repository

4. Install all the above mentioned specific packages

5. Execute the [Data Update Procedure](#heading=h.amimpgt6afn5) (see below)

6. Open one of the main Shiny App files: server.R, ui.R, global.R

7. Click on the ‘Run App’ button

8. *Optionally install other missing packages and re-run the application*

# Data Update Procedure

Download the following exports from CommCareHQ and copy them to the ‘data’ directory located at the root of the repository (create it if non existent).

<table>
  <tr>
    <th>Name</th>
    <th>Action</th>
  </tr>
  <tr>
    <th colspan="2">Case Exports</th>
  </tr>
  <tr>
    <td>tablet_user (IeDA Dashboard, PBF Migration)</td>
    <td><b>Rename to:</b> tablet-users.csv</td>
  </tr>
  <tr>
    <th colspan="2">Form Exports</th>
  </tr>
  <tr>
    <td>Child Treatment - Meta Data (IeDA Dashboard)</td>
    <td><b>Rename to:</b> child-treatment.csv</td>
  </tr>
  <tr>
    <td>Child Visit - Meta Data (IeDA Dashboard)</td>
    <td><b>Rename to:</b> child-visit.csv</td>
  </tr>
  <tr>
    <td>Enroll Child - Meta Data (IeDA Dashboard)</td>
    <td><b>Rename to:</b> enroll-child.csv</td>
  </tr>
  <tr>
    <th colspan="2">Other Exports</th>
  </tr>
  <tr>
    <td>List of mobile users</td>
    <td>
      <ul>
        <li><b>Rename column:</b> "data: csps_site_code" to “site_code”<br/></li>
        <li><b>Rename column:</b> “data: district” to “district”</li>
        <li><b>Keep only columns:</b> username, site_code, district</li>
        <li><b>Delete sheet:</b> groups</li>
        <li><b>Save as:</b> mobile-users.csv</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td>Locations</td>
    <td>
      <ul>
        <li><b>Keeps only sheet:</b> CSPS</li>
        <li><b>Keep only columns:</b> site_code, name, parent_site_code, latitude, longitude</li>
        <li><b>Save as:</b> locations.csv</td></li>
      </ul>
  </tr>
</table>


# Deploy procedure

The Shiny deployment toolchain is directly integrated with RStudio. Everything can be done through the RStudio UI.

## 1. Create a shinyapp.io account (only ones)

* Go to [https://www.shinyapps.io/admin/#/signup](https://www.shinyapps.io/admin/#/signup)

## 2. Connect your Shinyapps.io account to RStudio (only ones)

* Go to *Tools > ShinyApps > Manage Accounts*

* Click on *Connect*

* Follow instructions

## 3.Publish Application

* Open one of the main Shiny App files: server.R, ui.R, global.R

* Go to *Tools > Shiny App > Publish App...*

* Select account

* Select application (create new if first time)

