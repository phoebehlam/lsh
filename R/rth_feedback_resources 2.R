# Attachment 3F (IRB-approved) mental health resources, as HTML for the report.
# Wording is kept as in the attachment; edit here if the attachment is revised.

rth_resources <- list(
  intro = paste(
    "The research team is not trained or qualified to diagnose mental health",
    "disorders, such as anxiety and depression. If you are struggling with your",
    "mental health, you should seek professional help and treatment by contacting",
    "American Psychological Association (800-374-2721) or other licensed",
    "Psychiatric and Counseling services. Further resources are available at",
    "<a target=\"_blank\" rel=\"noopener\" href=\"https://www.apa.org/helpcenter\">apa.org/helpcenter</a>."),
  pittsburgh = list(
    list(name = "PA 2-1-1, United Way Hotline", hours = "24/7",
         contact = "Call 2-1-1 or 1-888-856-2773 &middot; <a target=\"_blank\" rel=\"noopener\" href=\"https://pa211sw.org/\">pa211sw.org</a>",
         what = "Connects people of Southwestern Pennsylvania to housing, community, health, and disaster services."),
    list(name = "resolve Crisis Services", hours = "24/7",
         contact = "Call 1-888-796-8226 &middot; <a target=\"_blank\" rel=\"noopener\" href=\"https://www.upmc.com/services/behavioral-health/resolve-crisis-services\">upmc.com/resolve</a>",
         what = "Provides crisis counseling and support by a trained counselor, information about how to get needed services, referral to additional crisis, treatment, and other behavioral health supports."),
    list(name = "Allegheny County Office of Children, Youth and Families (CYF)", hours = "24/7",
         contact = "Intake Office 412-473-2000 &middot; PA ChildLine 1-800-932-0313 &middot; <a target=\"_blank\" rel=\"noopener\" href=\"https://www.alleghenycounty.us/Services/Human-Services-DHS/DHS-Offices/Office-of-Children-Youth-and-Families\">alleghenycounty.us</a>",
         what = "Protects children, 18 years of age and younger, from abuse and neglect and works with families to provide a safe and loving permanent home for children.")
  ),
  national = list(
    list(name = "National suicide prevention lifeline", hours = "24/7",
         contact = "Call 988 &middot; <a target=\"_blank\" rel=\"noopener\" href=\"https://988lifeline.org/chat/\">Lifeline chat</a> &middot; <a target=\"_blank\" rel=\"noopener\" href=\"https://988lifeline.org/\">988lifeline.org</a>",
         what = "Provides confidential support for people in distress, prevention and crisis resources for you or your loved ones."),
    list(name = "Crisis Text Line", hours = "24/7",
         contact = "Text HOME to 741741 &middot; <a target=\"_blank\" rel=\"noopener\" href=\"https://www.crisistextline.org/\">crisistextline.org</a>",
         what = "Provides free and confidential text-based mental health support and crisis intervention."),
    list(name = "National Alliance on Mental Illness (NAMI) HelpLine", hours = "M-F, 10am to 10pm",
         contact = "Call 1-800-950-NAMI (6254) &middot; <a target=\"_blank\" rel=\"noopener\" href=\"https://www.nami.org/help\">nami.org/help</a>",
         what = "Offers a direct connection with another young person who shares similar experiences and is prepared to offer information, resources, and support to help you move through difficult times to a better place.")
  )
)

resources_html <- function(r = rth_resources) {
  item <- function(x) sprintf(
    '<div class="res"><div class="res-name">%s <span class="res-hours">%s</span></div><div class="res-contact">%s</div><div class="res-what">%s</div></div>',
    x$name, x$hours, x$contact, x$what)
  paste0(
    '<p>', r$intro, '</p>',
    '<h4>Pittsburgh</h4>', paste(vapply(r$pittsburgh, item, ""), collapse = ""),
    '<h4>National</h4>', paste(vapply(r$national, item, ""), collapse = ""))
}
