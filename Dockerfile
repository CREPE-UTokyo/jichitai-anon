# Use the r-base image as the base image
FROM r-base

# Install necessary Linux libraries
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libproj-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    git

# Create directory for R packages
RUN mkdir -p /my_packages

# Change the ownership of the directory to the 'staff' group which the R user is a part of
RUN chown :staff /my_packages

# Set the working directory
WORKDIR /usr/src/app

# Copy the entire local directory to the container
COPY ./ .
