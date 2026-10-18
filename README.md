This repo contains some code to evaluate decision models like Jev on several terminology tasks in OHDSI.

- Ariadne: Given a source term, select a target concept from a list of candidates (from embedding vector search)
- Phenelope: Given a concept set name and clinical definition, select zero, one, or more concepts to include from a list of candidate concepts (from Phoebe).
- Keeper: Given a disease name and a patient profile (data not in repo), decide if the patient is a case.

Code supports both local models or TypeSafe's API.