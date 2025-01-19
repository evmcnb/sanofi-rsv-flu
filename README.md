# sanofi-rsv-flu

This document outlines the workflow for contributing to the project, including how to create branches, submit pull requests, and collaborate efficiently without merging directly to `main`.

**Quick Navigation:**
- [Getting Started](#getting-started-with-the-project)
- [Branching Strategy](#making-changes-to-the-project)

---

# Getting Started with the Project

By the end of this, the project directory should have the same structure as this: 

![Imgur Image](https://i.imgur.com/fWPBTdJ.png)

### 1. Download/Clone the Repository

You can download or clone the repository using [Git](https://git-scm.com/downloads) or by downloading from Github.

- **To clone the repository using Git**:
  ```bash
  git clone https://github.com/evmcnb/BHF-ECG-detection.git
  ```

- **To download as a ZIP file**:
  Go to the repository page on GitHub and click the "Code" button, then select "Download ZIP." Extract the ZIP file to your desired location.

### 2. Install `virtualenv`

If you don't have `virtualenv` installed, you can install it using `pip`. Run the following command:

```bash
pip install virtualenv
```

### 3. Create a Virtual Environment

Navigate to the project folder and create a virtual environment:

```bash
cd /path/to/sanofi-rsv-flu
virtualenv venv
```

### 4. Activate the Virtual Environment

Before running the project, activate the virtual environment by following the instructions for your operating system:

- **macOS/Linux**:
  ```bash
  # Activate the virtual environment
  source venv/bin/activate
  ```

- **Windows (Command Prompt)**:
  ```cmd
  # Activate the virtual environment
  venv\Scripts\activate
  ```

- **Windows (PowerShell)**:
  ```powershell
  # Activate the virtual environment
  .\venv\Scripts\Activate
  ```

### 5. Install Requirements

After activating the virtual environment, install the necessary dependencies using `pip`:

```bash
pip install -r requirements.txt
```

### 6. Deactivating the Virtual Environment

When you're done working on the project, you can deactivate the virtual environment by running:

```bash
deactivate
```

This will allow everyone to run your code on different machines.


---

# Making changes to the project

Please read carefully about how to write and make changes to the project!

---

## 1. Branching Strategy

To keep the `main` branch stable and free from incomplete or experimental code, we use feature branches for all development work. The general rule is:

- **Never commit directly to `main`**.
- **Create a branch** for each feature, bug fix, or experiment.

### Branch naming convention:

- `feature/description` for new features (e.g., `feature/uk-processing`).
- `bugfix/description` for bug fixes (e.g., `bugfix/fix-image-loading`).
- `experiment/description` for experimental work (e.g., `experiment/shiny-app`).

## 2. Creating a New Branch

Follow these steps to create a new branch:

1. Make sure you are on the latest `main` branch:

   ```bash
   git checkout main
   git pull origin main
   ```

2. Create a new branch based on `main`:

   ```bash
   git checkout -b feature/your-branch-name
   ```

3. Push your new branch to the remote repository:
   ```bash
   git push origin feature/your-branch-name
   ```

Now, you can work on your feature or fix.

## 3. Making a Pull Request

Once you’ve completed your work on a branch, the next step is to submit a pull request (PR) to get it reviewed before merging it into `main`. Here’s how:

1. **Commit your changes** locally:

   ```bash
   git add .
   git commit -m "Descriptive message about what you did"
   ```

2. **Push your changes** to the remote repository:

   ```bash
   git push origin feature/your-branch-name
   ```

3. Go to the repository on GitHub, and you should see a prompt to create a pull request for your new branch.

4. Provide a **descriptive title** and **summary** of the changes made in your branch.

5. Assign at least one team member to **review** your PR.

6. Wait for feedback and make any necessary changes before the PR can be merged.

## 4. Code Review Process

Code review ensures code quality and that nothing breaks in `main`. Here's the process:

1. **Reviewers** will check the PR for:

   - Correctness and functionality.
   - Code style and clarity.
   - Potential performance improvements or refactoring opportunities.

2. If changes are required, the reviewer will leave comments. **Address the feedback** by making additional commits to your branch.

3. Once the reviewer is satisfied, they will **approve** the pull request.

## 5. Merging Approved Pull Requests

After your pull request is approved, you can merge it into `main`. Please follow this process:

1. Ensure your branch is up-to-date with `main`:

   ```bash
   git checkout main
   git pull origin main
   git checkout feature/your-branch-name
   git merge main
   ```

2. Resolve any merge conflicts that might arise.

3. On GitHub, click "Merge" once the pull request is approved and everything is up to date.

4. Delete your branch after the merge to keep the repository clean:
   ```bash
   git branch -d feature/your-branch-name
   git push origin --delete feature/your-branch-name
   ```

## 6. Best Practices

- **Write meaningful commit messages**: This makes it easier to track changes over time.
- **Test before pushing**: Ensure that the code works as expected locally before pushing your changes.
- **Use smaller pull requests**: Aim for small, focused pull requests that are easier to review.
- **Collaborate**: If you’re stuck or unsure about anything, communicate with the team. We're all in this together!

---

Thanks everyone for reading this and helping with the project!
