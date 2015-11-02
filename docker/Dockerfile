FROM node:4

MAINTAINER Dennis Gosnell <cdep.illabout@gmail.com>

# Update apt and instal some nice utilities.
RUN apt-get update
RUN apt-get update
RUN apt-get install tree sudo locales

# Set default locale.
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
RUN locale-gen
ENV LC_ALL en_US.UTF-8

# Username and group that everything will run under in the docker container.
ENV PURESCRIPT_DOCKER_USERNAME me
ENV PURESCRIPT_DOCKER_GROUP mygroup

# This adds a default uid and gid that we will use. This will map to the
# uid/gid on the host system, so if it does not match your current uid/gid, you
# need to change this.
#
# You can find out your uid/gid with the `id` command.
#
# In the future, there will be a feature added to docker that will simplify uid
# mappings:
# https://github.com/docker/docker/pull/12648
RUN groupadd --gid 1000 "${PURESCRIPT_DOCKER_GROUP}"
RUN useradd --gid "${PURESCRIPT_DOCKER_GROUP}" --create-home --shell /bin/bash --uid 1000 "${PURESCRIPT_DOCKER_USERNAME}"

# Make sure that our user can run sudo if need be.
RUN echo "${PURESCRIPT_DOCKER_USERNAME} ALL = NOPASSWD: ALL" >> /etc/sudoers

# Purescript binary version and hash.
ENV PURESCRIPT_VERSION v0.7.5
ENV PURESCRIPT_DOWNLOAD_SHA1 db40a6d9fd727eae210717a2fe81abd4d6ddd749

# Install purescript to /opt/purescript.
RUN cd /opt \
    && wget --no-verbose https://github.com/purescript/purescript/releases/download/${PURESCRIPT_VERSION}/linux64.tar.gz \
    && echo "${PURESCRIPT_DOWNLOAD_SHA1} linux64.tar.gz" | sha1sum --check - \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz
ENV PATH /opt/purescript:$PATH:/usr/local/lib/node_modules/pulp/node_modules/.bin

# Install pulp.
RUN npm install --global pulp

# This isn't really needed since we are not going to run pulp as root.
RUN echo '{ "allow_root": true }' > /root/.bowerrc

# Make sure that bower doesn't ask us if we want to send anonymous usage
# statistics.
RUN mkdir -p "/home/${PURESCRIPT_DOCKER_USERNAME}/.config/configstore"
RUN echo '{ "optOut": true }' > "/home/${PURESCRIPT_DOCKER_USERNAME}/.config/configstore/insight-bower.json"
RUN chown -R "${PURESCRIPT_DOCKER_USERNAME}:${PURESCRIPT_DOCKER_GROUP}" "/home/${PURESCRIPT_DOCKER_USERNAME}/.config"

CMD ["/bin/bash"]
