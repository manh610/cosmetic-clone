package com.cosmetic.gg.service.product.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.collection.ListUtils;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.product.Category;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.product.CategoryModel;
import com.cosmetic.gg.repository.product.CategoryRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.product.CategoryService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@AllArgsConstructor
public class CategoryServiceImpl implements CategoryService{

	private final CategoryRepository categoryRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Override
	public Map<String, Object> search(String keyword, String parentId, EStatus status, Integer pageIndex, Integer pageSize) {
		Map<String, Object> result = new HashMap<>();
		try {
			pageSize = pageSize == 0 ? CommonConstant.DEFAULT_PAGE_SIZE : pageSize;
			pageIndex = pageIndex > 0 ? (pageIndex - 1)*pageSize : 0;
			
			List<Category> users = categoryRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(parentId)) ? null : parentId,
					Objects.isNull(status) ? null : status.name(),
					pageIndex, pageSize);
			Integer totalItem = categoryRepository.cntCategory(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(parentId)) ? null : parentId,
					Objects.isNull(status) ? null : status.name());
			result.put("data", users);
			result.put("totalItem", totalItem);
			return result;
		}catch(Exception ex) {
			result.put("data", new ArrayList<Category>());
			result.put("totalItem", 0);
			log.error("Error while searching category", ex.getCause());
		}
		return result;
	}
	
	public CategoryModel getByKey(String key) {
		try {
			Category category = categoryRepository.findByKey(key);
		    return ModelMapper.map(category, CategoryModel.class);
		}catch(Exception ex) {
			log.error(String.format("Error while getting category by key: %s", key), ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Category getById(String id) {
		try {
			Category categoryEntity = categoryRepository.findById(id).orElse(null);
			if(categoryEntity == null || categoryEntity.getStatus() == EStatus.DELETED)
				categoryEntity = null;
			
			return categoryEntity;
		}catch(Exception ex) {
			log.error(String.format("Error while getting category by id: %s", id), ex.getCause());
		    return null;
		}
	}
	
	@Override
	public List<Error> validator(CategoryModel categoryModel){
		List<Error> errors = new ArrayList<>();
		try {
			Category categoryCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(categoryModel.getId()))) {
				if(categoryModel.getId().equals(categoryModel.getParentId()))
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Mã cha không hợp lệ", "Id of parent is not invalid"));
				
				categoryCheck = categoryRepository.findById(categoryModel.getId()).orElse(null);
				if (categoryCheck == null)
			          errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert categoryCheck != null;
				if(!categoryCheck.getCode().equals(categoryModel.getCode())) {
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
				}
				
				if(categoryCheck.getStatus() == EStatus.DELETED)
					errors.add(new Error().builder(ErrorCode.INVALID_STATUS));
			}
			
			if (Boolean.FALSE.equals(StringUtils.isNullOrEmpty(categoryModel.getParentId()))) {
				categoryCheck = categoryRepository.findById(categoryModel.getParentId()).orElse(null);
		        if (categoryCheck == null)
		          errors.add(new Error().builder(ErrorCode.INVALID_PARENT));
		    }
			
			categoryCheck = categoryRepository.findByKey(categoryModel.getCode());
		      if (categoryCheck != null && !categoryCheck.getId().equals(categoryModel.getId()))
		        errors.add(new Error().builder(ErrorCode.EXIST_CODE));
		}catch(Exception ex) {
			log.error("Error while validating category", ex.getCause());
		    errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public CategoryModel create(CategoryModel categoryModel) {
		try {
			Category categoryEntity = ModelMapper.map(categoryModel, Category.class);
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(Objects.requireNonNull(categoryEntity).getParentId()))) {
				Category category = getById(categoryEntity.getParentId());
				List<String> ancestors = new ArrayList<>(category.getAncestors());
				ancestors.add(category.getId());
				categoryEntity.setAncestors(ancestors);
			}else {
				categoryEntity.setAncestors(Collections.emptyList());
			}
			
			categoryEntity.prepareEntity();
			categoryEntity = categoryRepository.save(categoryEntity);
			if(Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryEntity.getId())))
				return null;
			
			CategoryModel result = ModelMapper.map(categoryEntity, CategoryModel.class);
			return result;
		}catch(Exception ex) {
			log.error("Error while creating category", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public CategoryModel update(CategoryModel categoryModel) {
		try {
			Category categoryEntityOrigin = getById(categoryModel.getId());
			String oldAncestors = String.join(";", categoryEntityOrigin.getAncestors());
			Category categoryEntity = ModelMapper.map(categoryModel, Category.class);
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(Objects.requireNonNull(categoryEntity).getParentId()))) {
				Category category = getById(categoryEntity.getParentId());
				List<String> ancestors = new ArrayList<>(category.getAncestors());
				ancestors.add(category.getId());
				categoryEntity.setAncestors(ancestors);
			}else {
				categoryEntity.setAncestors(Collections.emptyList());
			}
			
			categoryEntity.prepareEntity();
			categoryEntity = categoryRepository.save(categoryEntity);
			if(Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryEntity.getId())))
		        return null;
			
			List<Category> categories = categoryRepository.allChildren(categoryEntity.getId());
		      if (!categories.isEmpty() && !oldAncestors.equals(String.join(";", categoryEntity.getAncestors()))) {
		        String newAncestors = String.join(";", categoryEntity.getAncestors());
		        try {
		        	categoryRepository.bulkUpdateAncestorChildren(categoryEntity.getId(), oldAncestors, newAncestors);
		        } catch (Exception ex) {
		          log.error("Error while bulk updating ancestor category for children after updated", ex.getCause());
		        }
		      }
		      
		      CategoryModel result = ModelMapper.map(categoryEntity, CategoryModel.class);
		      return result;
		}catch(Exception ex) {
			log.error("Error while updating category", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public ErrorCode changeImage(MultipartFile imageFile, String id) {
		try {
			Category categoryEntity = getById(id);
			if (categoryEntity == null)
		        return ErrorCode.NOT_FOUND;
			
			if (categoryEntity.getStatus() == EStatus.DELETED)
		    	return ErrorCode.INVALID_STATUS;
			
			String originalFilename = imageFile.getOriginalFilename();
	        String extension = originalFilename.substring(originalFilename.lastIndexOf(".") + 1);
	        if (!StringUtils.isNullOrEmpty(originalFilename) && originalFilename.length() > 0) {
	        	if (!extension.equals("png") && !extension.equals("jpg") && !extension.equals("gif") 
	        			&& !extension.equals("svg") && !extension.equals("jpeg"))
	        		return ErrorCode.INVALID_IMAGE;
	        }
			
			byte[] img = imageFile.getBytes();
			if(img == null) {
				log.error("Error while convert image");
				return ErrorCode.ERROR_CONVERT_IMAGE;
			}
			
			categoryEntity.setImage(img);
			categoryEntity = categoryRepository.save(categoryEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryEntity.getId())))
				return ErrorCode.FAILURE;
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while deleting category", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public ErrorCode delete(String id) {
		try {
			Category categoryEntity = getById(id);
			if (categoryEntity == null)
		        return ErrorCode.NOT_FOUND;
			
			List<Category> categories = categoryRepository.allChildren(id);
		    if (!categories.isEmpty())
		        return ErrorCode.CHILD_REFERENCE;
		    
		    Product productCheck = productRepository.findByAttribute(id);
			if(productCheck != null) {
				return ErrorCode.CHILD_REFERENCE;
			}
		      
//		    if (categoryEntity.getStatus() == EStatus.DRAFT) {
//		    	categoryRepository.deleteById(id);
//		    	return ErrorCode.SUCCESS;
//			}
		      
		    categoryEntity.setStatus(EStatus.DELETED);
		    Category category = categoryRepository.save(categoryEntity);
		    return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while deleting category", ex.getCause());
		    return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public List<CategoryModel> children(String id, boolean isAll) {
		try {
			List<Category> categories;
			if (isAll)
				categories = categoryRepository.allChildren(id);
		    else
		    	categories = categoryRepository.children(id);
			return ListUtils.map(categories, x -> ModelMapper.map(x, CategoryModel.class));
		}catch(Exception ex) {
			log.error("Error while getting children category", ex.getCause());
		}
		return Collections.emptyList();
	}
	
	@Override
	  public List<CategoryModel> root() {
	    try {
	      List<Category> categories;
	      categories = categoryRepository.allRoot();
	      return ListUtils.map(categories, x -> ModelMapper.map(x, CategoryModel.class));
	    } catch (Exception ex) {
	      log.error("Error while getting all category root", ex.getCause());
	    }
	    return new ArrayList<>();
	}
	
	@Override
	public List<CategoryModel> buildTreeView() {
		try {
			List<CategoryModel> categories = ListUtils.map(categoryRepository.findAll(), x -> ModelMapper.map(x, CategoryModel.class));
			List<CategoryModel> categoryRoots = categories.stream()
			        .parallel()
			        .filter(x -> StringUtils.isNullOrEmpty(x.getParentId()))
			        .collect(Collectors.toList());
			
			for (CategoryModel menuModel : categoryRoots) {
		        menuModel.setChildren(getChildren(menuModel, categories));
		    }
			
			return categoryRoots.stream()
			        .sorted(Comparator.comparing(CategoryModel::getOffset).thenComparing(CategoryModel::getName))
			        .collect(Collectors.toList());
		}catch(Exception ex) {
			log.error("Error while building category with mode tree-view", ex.getCause());
		    return Collections.emptyList();
		}
	}
	
	@Override
	public List<CategoryModel> seedData(List<Category> entities) {
		try {
			List<Category> rs = new ArrayList<>();
			for (Category category : entities) {
				Category idpCategoryParent = categoryRepository.findByKey(category.getParentId());
				if (idpCategoryParent == null) {
					category.setParentId("");
					category.setAncestors(Collections.singletonList(category.getParentId()));
				}else {
					category.setParentId(idpCategoryParent.getId());
			        List<String> ancestors = new ArrayList<>(idpCategoryParent.getAncestors());
			        ancestors.add(idpCategoryParent.getId());
			        category.setAncestors(ancestors);
				}
				category.prepareEntity();
		        rs.add(categoryRepository.save(category));
			}
			return ListUtils.map(rs, x -> ModelMapper.map(x, CategoryModel.class));
		}catch(Exception ex) {
			log.error("Error seeding menu data", ex.getCause());
		    return Collections.emptyList();
		}
	}
	
	private List<CategoryModel> getChildren(CategoryModel categoryModel, List<CategoryModel> categories) {
		try {
			List<CategoryModel> categoryModelTemps = new ArrayList<>();
			Predicate<CategoryModel> predicateIsParent = x -> x.getParentId().equals(categoryModel.getId());
			List<CategoryModel> categoryModelChildren = categories.stream().parallel()
			        .filter(predicateIsParent)
			        .collect(Collectors.toList());
			
			for (int i = 0; i < categoryModelChildren.size(); i++) {
				categoryModelChildren.get(i).setChildren(new ArrayList<>());
		        String menuId = categoryModelChildren.get(i).getId();
		        
		        predicateIsParent = x -> x.getParentId().equals(menuId);
		        int childrenCount = (int) categories.parallelStream().filter(predicateIsParent).count();
		        if (childrenCount > 0) {
		        	categoryModelTemps = getChildren(categoryModelChildren.get(i), categories);
		        	categoryModelChildren.get(i).setChildren(categoryModelTemps);
		        }
		        
		        categoryModelTemps = categoryModelChildren;
			}
			return categoryModelTemps.stream()
			        .sorted(Comparator.comparing(CategoryModel::getOffset).thenComparing(CategoryModel::getName))
			        .collect(Collectors.toList());
		}catch(Exception ex) {
			log.error("Error while getting children category", ex.getCause());
			return Collections.emptyList();
		}
	}
}
