package com.cosmetic.gg.service.product;

import java.util.List;
import java.util.Map;

import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.product.Category;
import com.cosmetic.gg.model.product.CategoryModel;

public interface CategoryService {
	
	Map<String, Object> search(
			String keyword, 
			String parentId, 
			EStatus status, 
			Integer pageIndex, 
			Integer pageSize);
	
	Category getById(String id);

	List<Error> validator(CategoryModel categoryModel);
	
	CategoryModel create(CategoryModel categoryModel);
	
	CategoryModel update(CategoryModel categoryModel);
	
	ErrorCode changeImage(MultipartFile imageFile, String id);
	
	ErrorCode delete(String id);
	
	List<CategoryModel> children(String id, boolean isAll);
	
	List<CategoryModel> root();
	
	List<CategoryModel> buildTreeView();
	
	List<CategoryModel> seedData(List<Category> entities);
}
